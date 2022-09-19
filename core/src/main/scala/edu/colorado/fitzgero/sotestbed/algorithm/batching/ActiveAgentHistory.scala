package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

final case class ActiveAgentHistory(
  observedRouteRequestData: Map[String, AgentHistory] = Map.empty
) {

  def incrementReplannings(agentId: String): Either[Error, ActiveAgentHistory] = {
    this.observedRouteRequestData.get(agentId) match {
      case None =>
        val msg = s"attempting to increment replannings for agent $agentId who is " +
          "not found in the current active agent history"
        Left(new Error(msg))
      case Some(agentHistory) =>
        val updated = this.copy(
          observedRouteRequestData = this.observedRouteRequestData.updated(
            agentId,
            agentHistory.assignReplanningToCurrentTrip
          )
        )
        Right(updated)
    }
  }

  /**
    * adds another route request to the active history of route requests for this agent
    * @param data the new request
    * @return the updated history
    */
  def processRouteRequestData(
    data: RouteRequestData,
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  ): IO[ActiveAgentHistory] = {
    val agentId: String = data.request.agent

    val result = observedRouteRequestData.get(agentId) match {
      case None =>
        this.copy(observedRouteRequestData = observedRouteRequestData.updated(agentId, AgentHistory(data)))
      case Some(existingHistory) =>
        this.copy(observedRouteRequestData =
          observedRouteRequestData.updated(agentId, existingHistory.addRouteRequestData(data))
        )
    }
    IO.pure(result)
    // }

  }

  /**
    * true if this is the first time we have seen this agent in the network (i.e. start
    * of a trip)
    *
    * @param agentId the arriving agent
    * @return true if 1) the agent has a history, and 2) if that history
    *         only has one entry (aka no "tail")
    */
  def hasStartedRlEpisode(agentId: String): Boolean =
    this.observedRouteRequestData.get(agentId).exists { _.hasRlTrainingEpisodeStarted }

  /**
    * true if this is the first time we have seen this agent in the network (i.e. start
    * of a trip)
    *
    * @param agentId the arriving agent
    * @return true if 1) the agent has a history, and 2) if that history
    *         only has one entry (aka no "tail")
    */
  def isNewArrival(agentId: String): Boolean =
    this.observedRouteRequestData.get(agentId).exists { _.replanningEvents == 0 }

  /**
    * on arrival we can remove all unnecessary data associated with an agent, as it is no longer active
    * @param agentId the arriving agent
    * @return the updated history with this agent removed
    */
  def processArrivalFor(agentId: String): ActiveAgentHistory = {
    this.observedRouteRequestData.get(agentId) match {
      case None => this
      case Some(agentHistory) =>
        val updated = this.observedRouteRequestData.updated(agentId, agentHistory.catalogHistory)
        this.copy(
          observedRouteRequestData = updated
        )
    }
  }

  /**
    * gets the first request that we received for this agent, which should capture the original trip's distance
    * @param agentId the agent requested
    * @return the RouteRequestData if it exists
    */
  def getOldestRequest(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.originalRequest }

  /**
    * gets the most recent request we have received (as recent as the current time step)
    * @param agentId the agent requested
    * @return the latest RouteRequestData if we have any stored
    */
  def getNewestRequest(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.currentRequest }

  /**
    * gets the request before the current one
    * @param agentId the agent requested
    * @return the previous RouteRequestData before the current/newest one, if it exists
    */
  def getPreviousRequest(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).flatMap { _.previousRequest }

  /**
    * gets the most recent request that was responded to with a replanning route
    *
    * @param agentId the agent requested
    * @return the previous route request that was served, or none if none exists
    */
  def getPreviousReplanning(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).flatMap { _.mostRecentReplanning }

  /**
    * gets the complete history we have stored for an agent (as long as they are active in the system)
    * @param agentId the agent requested
    * @return the complete set of requests we have seen for this agent
    */
  def getOrderedRouteRequestHistory(agentId: String): Option[List[RouteRequestData]] =
    this.observedRouteRequestData.get(agentId).map { _.orderedRequestHistory }

  def getOldestRequestOrError(agentId: String): Either[Error, RouteRequestData] =
    getOldestRequest(agentId).toRight(new Error(s"agent $agentId history not stored"))

  def getNewestRequestOrError(agentId: String): Either[Error, RouteRequestData] =
    getNewestRequest(agentId).toRight(new Error(s"agent $agentId history not stored"))

  def getOrderedRouteRequestHistoryOrError(agentId: String): Either[Error, List[RouteRequestData]] =
    getOrderedRouteRequestHistory(agentId).toRight(new Error(s"agent $agentId history not stored"))

}

object ActiveAgentHistory {

  def NoHistory: ActiveAgentHistory = ActiveAgentHistory()
}
