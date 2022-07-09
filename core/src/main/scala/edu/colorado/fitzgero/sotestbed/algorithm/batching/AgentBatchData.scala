package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait AgentBatchData

object AgentBatchData {

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
    experiencedRoute: List[RouteRequestData.EdgeData],
    remainingRoute: List[RouteRequestData.EdgeData],
    remainingRouteDistance: Meters,
    lastReplanningTime: Option[SimTime]
  ) extends AgentBatchData {

    /**
      * report only travel times observed in the simulation, at the granularity of
      * completed links (no partial link traversals here)
      *
      * @return experienced travel time at time of request
      */
    def experiencedTravelTime: SimTime =
      experiencedRoute.flatMap { _.estimatedTimeAtEdge } match {
        case Nil   => SimTime.Zero
        case times => SimTime(times.map { _.value }.sum)
      }

    /**
      * combine observed travel times with the estimated travel time of the remaining route
      * against the current network conditions
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
  }

  object RouteRequestData {

    /**
      * attributes associated with an Edge traversal
      * @param edgeId
      * @param estimatedTimeAtEdge observation of agent time at this edge. zero if the agent
      *                            has not yet traversed this edge
      * @param linkSourceCoordinate
      * @param linkDestinationCoordinate
      */
    final case class EdgeData(
      edgeId: EdgeId,
      linkSourceCoordinate: Coordinate,
      linkDestinationCoordinate: Coordinate,
      estimatedTimeAtEdge: Option[SimTime] = None
    ) {

      def toPathSegment: PathSegment = {
        val cost = this.estimatedTimeAtEdge match {
          case None      => Cost.Zero
          case Some(est) => Cost(est.value)
        }
        PathSegment(edgeId, cost)
      }
    }
  }

  /**
    * removes an agent from the batching manager (closes routing session)
    *
    * @param agentId the agent's id to remove
    */
  final case class SOAgentArrivalData(agentId: String) extends AgentBatchData
}
