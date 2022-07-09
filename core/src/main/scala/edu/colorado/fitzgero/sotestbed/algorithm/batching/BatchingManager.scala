package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerResult
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

final case class BatchingManager(
  batchWindow: SimTime,
  minRequestUpdateThreshold: SimTime,
  costFunction: EdgeBPR => Cost,
  batchData: Map[String, RouteRequestData] = Map.empty,
  storedHistory: ActiveAgentHistory = ActiveAgentHistory(),
  mostRecentBatchRequested: SimTime = SimTime.Zero
) extends LazyLogging {

  /**
    * reads through the latest set of requests, and updates the collection of AgentBatchData
    * so that it is current. if any agent instructions stored are stale, we remove it.
    *
    * invariant: if an agent's data is stored in the manager but does not come via the updates,
    * then it is stale and removed.
    *
    * @param updates the updated AgentBatchData
    * @return the manager with all AgentBatchData updated, and stale instructions removed
    */
  def updateAgentBatchData(
    updates: List[AgentBatchData],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[BatchingManager] = {

    updates.foldLeft(IO.pure(this)) { (ioB, data) =>
      ioB.flatMap { b =>
        data match {
          case SOAgentArrivalData(agentId) =>
            // the agent is no longer in the system - remove
            val updated = b.copy(
              batchData = b.batchData - agentId,
              storedHistory = b.storedHistory.processArrivalFor(agentId)
            )
            IO.pure(updated)
          case routeRequestData: RouteRequestData =>
            val canUpdateRequest =
              b.storedHistory.getNewestData(routeRequestData.request.agent) match {
                case None =>
                  true
                case Some(mostRecent) =>
                  // guard against the requestUpdateCycle
                  routeRequestData.timeOfRequest - mostRecent.timeOfRequest >= minRequestUpdateThreshold
              }

            if (canUpdateRequest) {

              for {
                processed <- b.storedHistory.processRouteRequestData(routeRequestData, roadNetwork, costFunction)
              } yield this.copy(
                batchData = b.batchData.updated(routeRequestData.request.agent, routeRequestData),
                storedHistory = processed
              )
            } else IO.pure(b)

        }
      }
    }
  }

  /**
    * if it is the right time to request an SO batch of requests, then we do that
    *
    * detail: maybe we want to prevent instantaneous routing solutions in the future,
    * so we can demonstrate the online and realtime accuracy of the solution,
    * but for now, we can just let this happen, since the time a replanning
    * event is computed is less of a constraint under this formulation.
    *
    * @param currentTime the current sim time
    * @return either an SO route request (and batch manager update) or no change
    */
  def submitActiveRouteRequestsForReplanning(currentTime: SimTime): (BatchingManager, List[RouteRequestData]) = {
    BatchingManager.mostRecentBatchTime(this.batchWindow, currentTime) match {
      case None => (this, List.empty)
      case Some(aBatchTimePossiblySkipped) =>
        if (mostRecentBatchRequested < aBatchTimePossiblySkipped) {

          // we just passed a batch time or landed on one. export the batch. clear the batch of requests.
          val batchingManagerAfterExport: BatchingManager = this.copy(
            mostRecentBatchRequested = aBatchTimePossiblySkipped,
            batchData = Map.empty
          )
          (batchingManagerAfterExport, batchData.values.toList)
        } else {

          // haven't hit batch request time yet, so, hold on to the batch for now.
          (this, List.empty)
        }
    }
  }
}

object BatchingManager {

  def splitUEFromSO(data: AgentBatchData): Boolean = {
    data match {
      case data: AgentBatchData.RouteRequestData => data.request.requestClass == RequestClass.UE
      case _                                     => false
    }
  }

  /**
    * inspects the output of routing algorithms and makes sure there exists one response per agent
    * by only taking the best-cost solution for each agent
    * @param routingResult the result of independent routing algorithm calls for this [[SimTime]]
    * @return the route responses sent to the simulator
    */
  def resolveRoutingResultBatches(routingResult: List[RoutingAlgorithm.Result]): List[Response] =
    routingResult
      .flatMap { _.responses }
      .groupBy { _.request.agent }
      .map { case (_, solutionsForAgent: List[Response]) => solutionsForAgent.minBy { _.costEstimate } }
      .toList

  /**
    * gives us the most recently-completed batch window time
    * @param batchWindow duration of batches
    * @param currentTime the current sim time
    * @return the most recently-completed batch time, or None if currentTime is negative
    */
  def mostRecentBatchTime(batchWindow: SimTime, currentTime: SimTime): Option[SimTime] = {
    if (currentTime < SimTime.Zero) None
    else {
      val batchOfDay: Int = math.floor(currentTime.value.toDouble / batchWindow.value.toDouble).toInt
      Some { SimTime(batchOfDay) * batchWindow }
    }
  }

  /**
    * it is invalid to set a batch time for agents unless a complete
    * batch window exists between now and that time. this function
    * gives the next valid batch time based on the current time and
    * the batch window
    *
    * @param batchWindow configuration parameter
    * @param currentTime the current sim time
    * @return the next valid replanning batch time that can be set
    */
  def nextValidBatchingTime(batchWindow: SimTime, currentTime: SimTime): SimTime = {
    if (currentTime < SimTime.Zero) {
      // end of the first batch window will suffice for negative time values
      // (of which -1 should be the only one)
      batchWindow
    } else {
      // find the SimTime at the end of the next batch
      ((currentTime / batchWindow) * batchWindow) + (SimTime(2) * batchWindow)
    }
  }

  /**
    * if the updated strategy has invalid batch entries, list them
    * @param newStrategy the new strategy
    * @param batchWindow batch window for
    * @param currentTime the current sim time
    * @return a list of any entries found to be invalid (hopefully empty!)
    */
  def listInvalidStrategies[A](
    newStrategy: Map[SimTime, List[List[A]]],
    batchWindow: SimTime,
    currentTime: SimTime
  ): Seq[SimTime] = {
    for {
      t <- currentTime until nextValidBatchingTime(batchWindow, currentTime)
      time = SimTime(t)
      if newStrategy.isDefinedAt(time)
    } yield {
      time
    }
  }

  /**
    * keeps only the soonest batch, who should not be tampered with
    * @param oldStrat the old strategy
    * @param currentTime the current SimTime
    * @param batchWindow the batch window
    * @return the strategy with only the scheduled Requests for the nearest batch time
    */
  def retainStrategyForNearestBatchTime(
    oldStrat: Map[SimTime, List[List[AgentBatchData]]],
    currentTime: SimTime,
    batchWindow: SimTime
  ): Map[SimTime, List[List[AgentBatchData]]] = {
    val nearestBatchTime: SimTime = nextValidBatchingTime(batchWindow, currentTime)
    oldStrat.filter { case (simTime, _) => simTime == nearestBatchTime }
  }
}
