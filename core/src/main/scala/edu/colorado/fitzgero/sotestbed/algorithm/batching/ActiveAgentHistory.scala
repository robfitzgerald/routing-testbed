package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory.AgentHistory
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

    // for remaining route (un-traversed), store travel times based on
    // current network travel time estimates
    // must be done NOW since these estimates will change as the sim progresses
    val updatedDataIO = data.remainingRoute
      .traverse { e =>
        roadNetwork
          .edge(e.edgeId)
          .map {
            _.map { ea =>
              val estCost = SimTime(costFunction(ea.attribute).value.toLong)
              e.copy(estimatedTimeAtEdge = Some(estCost))
            }
          }
      }
      .map { withEstimates => data.copy(remainingRoute = withEstimates.flatten) }

    for {
      updatedData <- updatedDataIO
    } yield {

      observedRouteRequestData.get(agentId) match {
        case None =>
          this.copy(observedRouteRequestData = observedRouteRequestData.updated(agentId, AgentHistory(updatedData)))
        case Some(existingHistory) =>
          this.copy(observedRouteRequestData =
            observedRouteRequestData.updated(agentId, existingHistory.appendToTail(updatedData))
          )
      }
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
  def getOldestData(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.first }

  /**
    * gets the most recent request we have received (as recent as the current time step)
    * @param agentId the agent requested
    * @return the latest RouteRequestData if we have any stored
    */
  def getNewestData(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).map { _.current }

  /**
    * gets the trip before the current one
    * @param agentId the agent requested
    * @return the previous RouteRequestData before the current/newest one, if it exists
    */
  def getPreviousData(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).flatMap { _.previous }

  /**
    * gets the complete history we have stored for an agent (as long as they are active in the system)
    * @param agentId the agent requested
    * @return the complete set of requests we have seen for this agent
    */
  def getOrderedRouteRequestHistory(agentId: String): Option[List[RouteRequestData]] =
    this.observedRouteRequestData.get(agentId).map { _.orderedHistory }

  def getOldestDataOrError(agentId: String): Either[Error, RouteRequestData] =
    getOldestData(agentId).toRight(new Error(s"agent $agentId history not stored"))

  def getNewestDataOrError(agentId: String): Either[Error, RouteRequestData] =
    getNewestData(agentId).toRight(new Error(s"agent $agentId history not stored"))

  def getOrderedRouteRequestHistoryOrError(agentId: String): Either[Error, List[RouteRequestData]] =
    getOrderedRouteRequestHistory(agentId).toRight(new Error(s"agent $agentId history not stored"))
}

object ActiveAgentHistory {

  /**
    * tracks the data associated with an agent
    * @param first the first request we received
    * @param history all subsequent requests we have received, in reverse order for list prepend O(1) performance
    */
  final case class AgentHistory(first: RouteRequestData, history: List[RouteRequestData]) {

    def appendToTail(routeRequestData: RouteRequestData): AgentHistory =
      this.copy(
        history = routeRequestData +: history
      )

    def current: RouteRequestData = history.head

    def previous: Option[RouteRequestData] = history.tail.headOption

    def orderedHistory: List[RouteRequestData] = first +: history.reverse
  }

  object AgentHistory {
    def apply(first: RouteRequestData): AgentHistory = AgentHistory(first, List(first))
  }

  def NoHistory: ActiveAgentHistory = ActiveAgentHistory()
}
