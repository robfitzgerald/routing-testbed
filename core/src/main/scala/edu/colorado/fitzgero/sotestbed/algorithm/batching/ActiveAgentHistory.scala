package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}

final case class ActiveAgentHistory(
  observedRouteRequestData: Map[String, AgentHistory] = Map.empty
) {

  /**
    * adds another route request to the active history of route requests for this agent
    * @param data the new request
    * @return the updated history
    */
  def processRouteRequestData(data: RouteRequestData): ActiveAgentHistory = {
    val agentId: String = data.request.agent
    observedRouteRequestData.get(agentId) match {
      case None =>
        this.copy(observedRouteRequestData = observedRouteRequestData.updated(agentId, AgentHistory(data)))
      case Some(existingHistory) =>
        this.copy(observedRouteRequestData = observedRouteRequestData.updated(agentId, existingHistory.appendToTail(data)))
    }
  }

  /**
    * on arrival we can remove all data associated with an agent, as it is no longer active
    * @param agentId the arriving agent
    * @return the updated history with this agent removed
    */
  def processArrivalFor(agentId: String): ActiveAgentHistory = {
    this.copy(observedRouteRequestData = observedRouteRequestData - agentId)
  }

  /**
    * gets the first request that we received for this agent, which should capture the original trip's distance
    * @param agentId the agent requested
    * @return the RouteRequestData if it exists
    */
  def getOldestDataFor(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.head }

  /**
    * gets the most recent request we have received (as recent as the current time step)
    * @param agentId the agent requested
    * @return the latest RouteRequestData if we have any stored
    */
  def getMostRecentDataFor(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.last }

  /**
    * gets the complete history we have stored for an agent (as long as they are active in the system)
    * @param agentId the agent requested
    * @return the complete set of requests we have seen for this agent
    */
  def getOrderedRouteRequestHistoryFor(agentId: String): Option[List[RouteRequestData]] =
    this.observedRouteRequestData.get(agentId).map { _.orderedHistory }
}

object ActiveAgentHistory {

  /**
    * tracks the data associated with an agent
    * @param head the first request we received
    * @param tail all subsequent requests we have received, in reverse order for list prepend O(1) performance
    */
  final case class AgentHistory(head: RouteRequestData, tail: List[RouteRequestData] = List.empty) {

    def appendToTail(routeRequestData: RouteRequestData): AgentHistory =
      this.copy(
        tail = routeRequestData +: tail
      )

    def last: RouteRequestData =
      tail match {
        case Nil => head
        case _   => tail.head
      }
    def orderedHistory: List[RouteRequestData] = head +: tail.reverse
  }

  def NoHistory: ActiveAgentHistory = ActiveAgentHistory()
}
