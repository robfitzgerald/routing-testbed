package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching._
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

final case class BatchingManager(
  batchWindow: SimTime,
  requestUpdateCycle: SimTime,
  minBatchSize: Int,
  batchData: Map[String, RouteRequestData] = Map.empty,
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
  def updateAgentBatchData(updates: List[AgentBatchData]): BatchingManager = {
    val updated: Map[String, RouteRequestData] =
      updates.foldLeft(this.batchData) { (b, data) =>
        data match {
          case SOAgentArrivalData(agentId) => b - agentId
          case routeRequestData: RouteRequestData => b.updated(routeRequestData.request.agent, routeRequestData)
        }
      }
    this.copy(batchData = updated)
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
          // we just passed a batch time or landed on one
          (this.copy(mostRecentBatchRequested = aBatchTimePossiblySkipped), batchData.values.toList)
        } else {
          // haven't hit it yet
          (this, List.empty)
        }
    }
  }



//  /**
//    * reads through the latest set of requests, and updates the collection of AgentBatchData
//    * so that it is current. if any agent instructions stored are stale, we remove it.
//    *
//    * invariant: if an agent's data is stored in the manager but does not come via the updates,
//    * then it is stale and removed.
//    *
//    * @param updates the updated AgentBatchData
//    * @param currentTime the current time
//    * @return the manager with all AgentBatchData updated, and stale instructions removed
//    */
//  def updateStoredBatchData(updates: List[AgentBatchData], currentTime: SimTime): BatchingManager = {
//
//    val isNotStaleTest: AgentBatchData => Boolean = {
//      val mostRecentRequestUpdate: SimTime = currentTime - (currentTime % requestUpdateCycle)
//      data: AgentBatchData => { mostRecentRequestUpdate < data.timeOfRequest }
//    }
//
//    // update list of agents in simulation
//    val newData: Map[String, AgentBatchData] = updates.map { a =>
//      a.request.agent -> a
//    }.toMap
//    val cleanedOldData: Map[String, AgentBatchData]         = this.agentBatchDataMap.filter { case (_, data) => isNotStaleTest(data) }
//    val updatedAgentBatchDatas: Map[String, AgentBatchData] = cleanedOldData ++ newData
//
//    // remove stale instructions
//    val (cleanedBatchingStrategy: BatchingStrategy, cntRemoved: Int) =
//      updatedAgentBatchDatas.foldLeft((Map.empty[String, BatchingInstruction], 0)) {
//        case ((strat, countRemoved), (agentId, _)) =>
//          this.batchingStrategy.get(agentId) match {
//            case None    => (strat - agentId, countRemoved + 1)
//            case Some(_) => (strat, countRemoved)
//          }
//      }
//
//    logger.debug(s"removed $cntRemoved stale batching strategies")
//
//    this.copy(agentBatchDataMap = updatedAgentBatchDatas, batchingStrategy = cleanedBatchingStrategy)
//  }

//  def updateStoredBatchData(updates: List[AgentBatchData], currentTime: SimTime): BatchingManager = {
//
//    val updatesMap: Map[String, AgentBatchData] = updates.map{a => a.request.agent -> a}.toMap
//    val isNotStaleTest: BatchingInstruction => Boolean = BatchingManager.isNotStaleRequest(currentTime, this.requestUpdateCycle)
//
//    // update agent with newest received data. if no agent data was received, remove
//    // the agent from the batching strategy (they must have arrived at their destination).
//    val updatedStrat: Map[String, BatchingInstruction] = for {
//      (agentId, instruction) <- this.batchingStrategy
//      updatedAgentBatchDataOpt = updatesMap.get(agentId)
//      if updatedAgentBatchDataOpt.isDefined ||            // we received an update for this agent
//        isNotStaleTest(instruction)
//    } yield {
//      // we either received new data, or if not, we have determined that the data isn't stale
//      updatedAgentBatchDataOpt match {
//        case None =>
//          // add new agent to the strategy
//          agentId -> instruction
//        case Some(updatedData) =>
//          // update stored agent data in strategy without disturbing any batch assignments
//          agentId -> instruction.copy(agentBatchData = updatedData)
//      }
//    }
//
//    val staleRequests: Int = this.batchingStrategy.size - updatedStrat.size
//    if (staleRequests > 0) {
//      logger.debug(s"removing $staleRequests stale requests")
//    }
//
//    this.copy(batchingStrategy = updatedStrat)
//  }

//  /**
//    * takes an update to the batching strategy and folds it into the strategy already defined
//    *
//    * @param revisedBatchingInstructions instructions from a batching function
//    * @param currentTime the current time
//    * @return the manager with any updated (valid) instructions folded into it's batching strategy
//    */
//  def applyBatchingFunctionInstructions(
//    revisedBatchingInstructions: Option[List[BatchingInstruction]],
//    currentTime: SimTime
//  ): BatchingManager = {
//
//    // resolves new with old, ignoring any changes to the next batch time. if no new strat, keep old one
//    revisedBatchingInstructions match {
//      case None                       => this
//      case Some(batchingInstructions) =>
//        // create a list of the invalid times
//        val invalidTimes: Set[SimTime] = {
//          for {
//            t <- currentTime until BatchingManager.nextValidBatchingTime(batchWindow, currentTime)
//            time = SimTime(t)
//          } yield time
//
//        }.toSet
//
//        // split out invalid and valid instructions
//        val (invalidInstructions, validInstructions) = batchingInstructions.partition { i =>
//          invalidTimes.contains(i.batchingTime)
//        }
//
//        // report invalid instructions
//        for { i <- invalidInstructions } {
//          logger.debug(s"batching strategy update at time $currentTime has invalid time ${i.batchingTime} for agent ${i.agentId}; ignoring")
//        }
//
//        // create/update batching strategy entries for agents based on the valid instructions
//        val updatedBatchingStrategy: BatchingStrategy =
//          this.batchingStrategy ++ validInstructions.map { i =>
//            i.agentId -> i
//          }.toMap
//
//        this.copy(batchingStrategy = updatedBatchingStrategy)
//    }
//  }
//
//  /**
//    * exports any batching instructions for the current time as batches of routing requests
//    * @param currentSimTime the current time
//    * @return the updated batching manager as well as any batches of routing requests to solve
//    */
//  def getBatchesForTime(currentSimTime: SimTime): (BatchingManager, List[List[Request]]) = {
//    // returns our strat for currentSimTime
//
//    // separate out instructions for this batch and later batches
//    val (thisBatch, futureBatches) =
//      this.batchingStrategy.values
//        .foldLeft((List.empty[BatchingInstruction], List.empty[BatchingInstruction])) {
//          case ((t, f), i) =>
//            // catch any batching times since the last simulation update which were not included in the
//            // most recent batch request (catches batches which should be handled but their exact SimTime
//            // was skipped by the underlying simulator
//            if (this.mostRecentBatchRequested < i.batchingTime && i.batchingTime <= currentSimTime) (i +: t, f)
//            else (t, i +: f)
//        }
//
//    // group into sub-batches of Request objects
//    val subBatches: List[List[Request]] =
//      thisBatch
//        .groupBy { _.batchId }
//        .values
//        .map {
//          _.flatMap { i =>
//            this.agentBatchDataMap.get(i.agentId) match {
//              case None                 => None
//              case Some(agentBatchData) => Some { agentBatchData.request }
//            }
//          }
//        }
//        .toList
//
//    // re-wrap the remaining instructions as the next BatchingStrategy
//    val nextStrategy: BatchingStrategy = futureBatches.map { i =>
//      i.agentId -> i
//    }.toMap
//
//    val updatedBatchingManager: BatchingManager = this.copy(
//      mostRecentBatchRequested = currentSimTime,
//      batchingStrategy = nextStrategy
//    )
//
//    (updatedBatchingManager, subBatches)
//  }
}

object BatchingManager {

  def splitUEFromSO(data: AgentBatchData): Boolean = {
    data match {
      case AgentBatchData.RouteRequestData(req, _, _, _) => req.requestClass == RequestClass.UE
      case _ => false
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
    * it is invalid to set a batch time for agents unless a complete
    * batch window exists between now and that time. this function
    * gives the next valid batch time based on the current time and
    * the batch window
    *
    * @param batchWindow configuration parameter
    * @param currentTime the current sim time
    * @return the next valid replanning batch time that can be set
    */
  def nextBatchTimeFrom(batchWindow: SimTime, currentTime: SimTime): SimTime = {
    if (currentTime < SimTime.Zero) {
      // end of the first batch window will suffice for negative time values
      // (of which -1 should be the only one)
      SimTime.Zero
    } else {
      // find the SimTime at the end of the next batch
      ((currentTime / batchWindow) * batchWindow) + batchWindow + SimTime(1)
    }
  }

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
      Some{ SimTime(batchOfDay) * batchWindow }
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
  def listInvalidStrategies[A](newStrategy: Map[SimTime, List[List[A]]], batchWindow: SimTime, currentTime: SimTime): Seq[SimTime] = {
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

//  /**
//    * removes stale data from the set of instructions
//    *
//    * @param currentTime the current time
//    * @param requestUpdateCycle the cycle that updates are updated
//    * @param instruction an instruction to test
//    * @return
//    */
//  def isNotStaleRequest(
//    currentTime: SimTime,
//    requestUpdateCycle: SimTime)(
//    instruction: BatchingInstruction): Boolean = {
//
//    val mostRecentRequestUpdate: SimTime = currentTime - (currentTime % requestUpdateCycle)
//    mostRecentRequestUpdate < instruction.agentBatchData.timeOfRequest
//  }
}
