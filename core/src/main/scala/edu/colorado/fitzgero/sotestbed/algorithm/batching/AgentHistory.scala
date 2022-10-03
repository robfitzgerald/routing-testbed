package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData._

/**
  * tracks the request data associated with an agent, noting which requests were served
  * with a replanning route. at the end of their trip, we retain knowledge of only their first and last
  * request data and the metadata of their trip.
  *
  * @param first the first request we received
  * @param history all subsequent requests we have received, in reverse order for list prepend O(1) performance
  * @param replanningEvents number of times agent route was replanned
  * @param hasRlTrainingEpisodeStarted true if we have notified the RL server that this agent episode has begun
  * @param finalized true when the agent finishes their trip
  */
final case class AgentHistory(
  first: AgentBatchData.EnterSimulation,
  history: List[AgentHistory.Entry] = List.empty,
  replanningEvents: Int = 0,
  uoPathsAssigned: Int = 0,
  hasRlTrainingEpisodeStarted: Boolean = false,
  finalized: Boolean = false
) {

  def uoAssignmentRate: Double =
    if (replanningEvents == 0) 0.0 else uoPathsAssigned.toDouble / replanningEvents.toDouble

  /**
    * adds another route request to this history
    *
    * @param routeRequestData
    * @return
    */
  def addRouteRequestData(routeRequestData: RouteRequestData): AgentHistory =
    this.copy(
      history = AgentHistory.Entry(routeRequestData) +: history
    )

  /**
    * modifies this history, noting that the current trip was provided
    * a replanning route
    *
    * @return updated history
    */
  def assignReplanningToCurrentTrip: Either[Error, AgentHistory] = history match {
    case Nil =>
      Left(new Error("no trip to assign replanning to"))
    case head :: tail =>
      val updated = this.copy(
        replanningEvents = this.replanningEvents + 1,
        history = head.copy(replanned = true) +: tail
      )
      Right(updated)
  }

  /**
    * modifies this history, noting that the current trip was provided
    * a replanning route
    *
    * @return updated history
    */
  def assignUoRouteToCurrentTrip: Either[Error, AgentHistory] = history match {
    case Nil =>
      Left(new Error("no trip to assign replanning to"))
    case head :: tail =>
      val updated = this.copy(
        uoPathsAssigned = this.uoPathsAssigned + 1
      )
      Right(updated)
  }

  /**
    * gets the earliest request which was also assigned a
    * replanning route
    *
    * @return the earliest request that was replanned, or None
    * if no requests have been served a route
    */
  def originalReplanning: Option[RouteRequestData] =
    this.history.reverse.find(_.replanned).map { _.data }

  def mostRecentRequest: Option[RouteRequestData] = history.headOption.map { _.data }

  /**
    * the current request is the most-recently added to the AgentHistory. if the
    * AgentHistory.history is empty, this is then the AgentHistory.first entry,
    * otherwise it's the head of the history (reverse order)
    *
    * @return the current trip
    */
  def currentRequest: Either[Error, RouteRequestData] =
    mostRecentRequest.toRight(new Error("no current request stored"))

  /**
    * the request before the current one, if it exists
    *
    * @return the previous request or None if there has only been one request
    */
  def previousRequest: Option[RouteRequestData] = history.tail.headOption.map { _.data }

  /**
    * finds the most recent replanning event that has happened
    *
    * @return the most recent request that was served a replanning route,
    * or None if no requests were served routes
    */
  def mostRecentReplanning: Option[RouteRequestData] = this.history.find(_.replanned).map { _.data }

  /**
    * all requests for this agent, in the order they arrived
    *
    * @return requests
    */
  def orderedRequestHistory: List[RouteRequestData] = orderedEntryHistory.map(_.data)

  /**
    * any requests for this agent that were served replanning routes
    * in the order that they arrived
    *
    * @return replanned requests
    */
  def orderedReplanHistory: List[RouteRequestData] = orderedEntryHistory.filter(_.replanned).map(_.data)

  def orderedEntryHistory: List[AgentHistory.Entry] = this.history.reverse

  def catalogHistory: AgentHistory = this.copy(finalized = true, history = this.history.take(1))
}

object AgentHistory {

  /**
    * internal data class for holding a single request
    *
    * @param data request to hold
    * @param replanned whether this request was replanned, assumed false
    * until set later at the end of the routing process
    */
  final case class Entry(data: RouteRequestData, replanned: Boolean = false)

  /**
    * create an AgentHistory, initialized with the first request for an agent
    *
    * @param first the first request seen for some agent
    * @return the initial state of an AgentHistory
    */
  def apply(enterSimulationMessage: AgentBatchData.EnterSimulation): AgentHistory =
    AgentHistory(enterSimulationMessage, List.empty)
}
