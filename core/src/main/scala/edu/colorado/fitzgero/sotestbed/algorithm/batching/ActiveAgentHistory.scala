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

  def getAgentHistory(agentId: String): Option[AgentHistory] =
    this.observedRouteRequestData.get(agentId)

  def getAgentHistoryOrError(agentId: String): Either[Error, AgentHistory] =
    getAgentHistory(agentId).toRight(new Error(s"expected agent history for agent $agentId doesn't exist"))

  def hasHistoryForAgent(agentId: String): Boolean = getAgentHistory(agentId).isDefined

  def incrementReplannings(agentId: String, assignedUoTrip: Boolean): Either[Error, ActiveAgentHistory] = {
    this.observedRouteRequestData.get(agentId) match {
      case None =>
        val msg = s"attempting to increment replannings for agent $agentId who is " +
          "not found in the current active agent history"
        Left(new Error(msg))
      case Some(agentHistory) =>
        val updateResult = for {
          h1 <- agentHistory.assignReplanningToCurrentTrip
          h2 <- if (assignedUoTrip) h1.assignUoRouteToCurrentTrip else Right(h1)
        } yield h2

        updateResult.map { hist =>
          this.copy(
            observedRouteRequestData = this.observedRouteRequestData.updated(
              agentId,
              hist
            )
          )
        }

    }
  }

  def incrementUoAssignments(agentId: String): Either[Error, ActiveAgentHistory] = {
    this.observedRouteRequestData.get(agentId) match {
      case None =>
        val msg = s"attempting to increment replannings for agent $agentId who is " +
          "not found in the current active agent history"
        Left(new Error(msg))
      case Some(agentHistory) =>
        agentHistory.assignReplanningToCurrentTrip.map { updatedHistory =>
          this.copy(
            observedRouteRequestData = this.observedRouteRequestData.updated(
              agentId,
              updatedHistory
            )
          )
        }
    }
  }

  /**
    * adds another route request to the active history of route requests for this agent
    * @param data the new request
    * @return the updated history
    */
  def processRouteRequestData(
    data: RouteRequestData
  ): Either[Error, ActiveAgentHistory] = {
    val agentId: String = data.request.agent

    observedRouteRequestData.get(agentId) match {
      case None =>
        Left(new Error("attempting to add route request data before agent history was intialized"))
      case Some(existingHistory) =>
        val updated = this.copy(observedRouteRequestData =
          observedRouteRequestData.updated(agentId, existingHistory.addRouteRequestData(data))
        )
        Right(updated)
    }
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

  def processDepartureFor(enterSim: AgentBatchData.EnterSimulation): Either[Error, ActiveAgentHistory] = {
    if (this.observedRouteRequestData.isDefinedAt(enterSim.agent)) {
      Left(new Error(s"agent ${enterSim.agent} departing but already in agent history"))
    } else {
      val updated = this.copy(
        observedRouteRequestData = this.observedRouteRequestData.updated(enterSim.agent, AgentHistory(enterSim))
      )
      Right(updated)
    }
  }

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
  def getOldestData(agentId: String): Option[AgentBatchData.EnterSimulation] =
    this.observedRouteRequestData.get(agentId).map { _.first }

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

  def getOldestDataOrError(agentId: String): Either[Error, AgentBatchData.EnterSimulation] =
    getOldestData(agentId).toRight(new Error(s"agent $agentId history not stored"))

  def getNewestRequest(agentId: String): Option[RouteRequestData] =
    this.observedRouteRequestData.get(agentId).flatMap { _.mostRecentRequest }

  def getNewestDataOrError(agentId: String): Either[Error, RouteRequestData] =
    getNewestRequest(agentId)
      .toRight(new Error(s"agent $agentId not found in history"))

  def getOrderedRouteRequestHistoryOrError(agentId: String): Either[Error, List[RouteRequestData]] =
    getOrderedRouteRequestHistory(agentId).toRight(new Error(s"agent $agentId history not stored"))

}

object ActiveAgentHistory {

  def NoHistory: ActiveAgentHistory = ActiveAgentHistory()
}
