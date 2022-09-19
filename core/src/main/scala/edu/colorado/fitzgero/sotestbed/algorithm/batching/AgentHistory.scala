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
  first: AgentHistory.Entry,
  history: List[AgentHistory.Entry],
  replanningEvents: Int = 0,
  hasRlTrainingEpisodeStarted: Boolean = false,
  finalized: Boolean = false
) {

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
  def assignReplanningToCurrentTrip: AgentHistory = history match {
    case Nil =>
      this.copy(
        replanningEvents = this.replanningEvents + 1,
        first = this.first.copy(
          replanned = true
        )
      )
    case head :: tail =>
      this.copy(
        replanningEvents = this.replanningEvents + 1,
        history = head.copy(replanned = true) +: tail
      )
  }

  /**
    * gets the original request from the history
    *
    * @return the first request submitted for this agent
    */
  def originalRequest: RouteRequestData = this.first.data

  /**
    * gets the earliest request which was also assigned a
    * replanning route
    *
    * @return the earliest request that was replanned, or None
    * if no requests have been served a route
    */
  def originalReplanning: Option[RouteRequestData] =
    if (this.first.replanned) Some(this.first.data)
    else this.history.reverse.find(_.replanned).map { _.data }

  /**
    * the current request is the most-recently added to the AgentHistory. if the
    * AgentHistory.history is empty, this is then the AgentHistory.first entry,
    * otherwise it's the head of the history (reverse order)
    *
    * @return the current trip
    */
  def currentRequest: RouteRequestData = history match {
    case Nil       => this.first.data
    case head :: _ => head.data
  }

  /**
    * the request before the current one, if it exists
    *
    * @return the previous request or None if there has only been one request
    */
  def previousRequest: Option[RouteRequestData] = history match {
    case Nil               => None
    case head :: Nil       => Some(this.first.data)
    case head :: prev :: _ => Some(prev.data)
  }

  /**
    * finds the most recent replanning event that has happened
    *
    * @return the most recent request that was served a replanning route,
    * or None if no requests were served routes
    */
  def mostRecentReplanning: Option[RouteRequestData] =
    this.history.find(_.replanned) match {
      case None             => if (this.first.replanned) Some(this.first.data) else None
      case Some(mostRecent) => Some(mostRecent.data)
    }

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

  def orderedEntryHistory: List[AgentHistory.Entry] = {
    val tail = this.history.reverse.filter(_.replanned)
    if (this.first.replanned) this.first +: tail else tail
  }

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
  def apply(first: RouteRequestData): AgentHistory = AgentHistory(Entry(first), List.empty)
}
