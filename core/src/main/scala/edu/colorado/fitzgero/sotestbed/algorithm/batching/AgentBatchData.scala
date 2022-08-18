package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv.HeaderDecoder

sealed trait AgentBatchData

object AgentBatchData {

  case class EnterSimulation(
    agent: String,
    departureTime: SimTime
  ) extends AgentBatchData

  /**
    * captures the data about an agent's request which is meaningful for batch optimization and routing
    *
    * @param request a request for routing from the agent's current location to their destination
    * @param timeOfRequest the SimTime that the request was generated
    * @param experiencedRoute a route with experienced link traversal times
    * @param remainingRoute a route plan with no experienced times
    * @param lastReplanningTime the most recent replanning SimTime if the agent was previously re-planned
    */
  final case class RouteRequestData(
    request: Request,
    timeOfRequest: SimTime,
    experiencedTravelTime: SimTime,
    experiencedRoute: List[EdgeData],
    remainingRoute: List[EdgeData],
    remainingRouteDistance: Meters,
    lastReplanningTime: Option[SimTime]
  ) extends AgentBatchData {

    // /**
    //   * report only travel times observed in the simulation, at the granularity of
    //   * completed links (no partial link traversals here)
    //   *
    //   * @return experienced travel time at time of request
    //   */
    // def experiencedTravelTime: SimTime =
    //   experiencedRoute.flatMap { _.estimatedTimeAtEdge } match {
    //     case Nil   => SimTime.Zero
    //     case times => SimTime(times.map { _.value }.sum)
    //   }

    /**
      * combine observed travel times with the estimated travel time of the remaining route
      * against the current network conditions via the provided cost function
      *
      * @param roadNetwork the current network conditions
      * @param costFunction cost function to use when estimating travel time
      * @return
      */
    def overallTravelTimeEstimate(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      costFunction: EdgeBPR => Cost
    ): IO[SimTime] = {
      val ttExperienced: List[Long] = experiencedRoute.flatMap { _.estimatedTimeAtEdge.map { _.value } }
      val ttRemainingIO: IO[List[Long]] = remainingRoute
        .traverse { e =>
          roadNetwork.edge(e.edgeId).map {
            case None     => None
            case Some(ea) => Some(costFunction(ea.attribute).value.toLong)
          }
        }
        .map { _.flatten }

      val result = for {
        ttRemaining <- ttRemainingIO
      } yield {
        val ttList = ttExperienced ++ ttRemaining
        if (ttList.nonEmpty) SimTime(ttList.sum) else SimTime.Zero
      }

      result
    }

    /**
      * combine observed travel times with the estimated travel time of the remaining route
      * against the current network conditions as reported by the simulation. all
      * link data must have a travel time value associated with it otherwise this function
      * will return an error
      *
      * @return
      */
    def overallTravelTimeEstimate: Either[Error, SimTime] = {
      val links = (experiencedRoute ::: remainingRoute).zipWithIndex
      RouteRequestData.ttEstimate(links)
    }

    def remainingTravelTimeEstimate: Either[Error, SimTime] = {
      RouteRequestData.ttEstimate(remainingRoute.zipWithIndex)
    }
  }

  object RouteRequestData {

    def ttEstimate(links: List[(EdgeData, Int)]): Either[Error, SimTime] = {
      links
        .traverse {
          case (edgeData, idx) =>
            edgeData.estimatedTimeAtEdge match {
              case None =>
                val link = s"${edgeData.edgeId} (route index $idx)"
                Left(new Error(s"travel time not reported for link $link"))
              case Some(simTime) =>
                Right(simTime)
            }
        }
        .map { _.foldLeft(SimTime.Zero) { _ + _ } }
    }
  }

  /**
    * removes an agent from the batching manager (closes routing session)
    *
    * @param agentId the agent's id to remove
    */
  final case class SOAgentArrivalData(
    agentId: String,
    departureTime: SimTime,
    arrivalTime: SimTime,
    finalTravelTime: SimTime,
    finalDistance: Meters
  ) extends AgentBatchData {
    override def toString = f"$agentId,$departureTime,$arrivalTime,$finalTravelTime,$finalDistance"
  }

  object SOAgentArrivalData {
    val Columns = ("agentId", "departureTime", "arrivalTime", "finalTravelTime", "finalDistance")
    def Header  = Columns.toList.mkString(",")

    implicit val hd: HeaderDecoder[SOAgentArrivalData] = HeaderDecoder.decoder(
      "agentId",
      "departureTime",
      "arrivalTime",
      "finalTravelTime",
      "finalDistance"
    ) { SOAgentArrivalData.apply }
  }
}
