package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching._
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime


final case class BM_2(
  batchWindow       : SimTime,
  requestUpdateCycle: SimTime,
  minBatchSize      : Int,
  batchingStrategy  : BatchingStrategy = Map.empty,
) extends LazyLogging {

  /**
    * reads through the latest set of requests, and updates the collection of AgentBatchData
    * so that it is current. if any agent instructions stored are stale, we remove it.
    *
    * invariant: if an agent's data is stored in the manager but does not come via the updates,
    * then it is stale and removed.
    *
    * @param updates the updated AgentBatchData
    * @param currentTime the current time
    * @return the manager with all AgentBatchData updated, and stale instructions removed
    */
  def updateStoredBatchData(updates: List[AgentBatchData], currentTime: SimTime): BM_2 = {
    // remove requests that are stale; replace AgentBatchData in BatchingStrategy if provided

    val updatesMap: Map[String, AgentBatchData] = updates.map{a => a.request.agent -> a}.toMap
    val isNotStaleTest: BatchingInstruction => Boolean = BatchingManager.isNotStaleRequest(currentTime, this.requestUpdateCycle)

    val updatedStrat: Map[String, BatchingInstruction] = for {
      (agentId, instruction) <- this.batchingStrategy
      updatedAgentBatchDataOpt = updatesMap.get(agentId)
      if updatedAgentBatchDataOpt.isDefined || isNotStaleTest(instruction)
    } yield {
      updatedAgentBatchDataOpt match {
        case None => agentId -> instruction
        case Some(updatedData) => agentId -> instruction.copy(agentBatchData = updatedData)
      }
    }

    val staleRequests: Int = this.batchingStrategy.size - updatedStrat.size
    if (staleRequests > 0) {
      logger.debug(s"removing $staleRequests stale requests")
    }

    this.copy(batchingStrategy = updatedStrat)
  }

  /**
    * takes an update to the batching strategy and folds it into the strategy already defined
    *
    * @param revisedBatchingInstructions instructions from a batching function
    * @param currentTime the current time
    * @return the manager with any updated (valid) instructions folded into it's batching strategy
    */
  def updateBatchingStrategy(revisedBatchingInstructions: Option[List[BatchingInstruction]], currentTime: SimTime): BM_2 = {

    // remove requests that are stale
    val cleanedOldStrategy: BatchingStrategy =
      this.batchingStrategy.filter{ case (_, batchingInstruction) =>
        val mostRecentRequestUpdate: SimTime = currentTime - (currentTime % requestUpdateCycle)
        mostRecentRequestUpdate < batchingInstruction.agentBatchData.timeOfRequest
      }

    // resolves new with old, ignoring any changes to the next batch time
    // if no new strat, keep old one (but filter old requests)
    revisedBatchingInstructions match {
      case None => this.copy(batchingStrategy = cleanedOldStrategy)
      case Some(batchingInstructions) =>

        // create a list of the invalid times
        val invalidTimes: Set[SimTime] = {
          for {
            t <- currentTime until BatchingManager.nextValidBatchingTime(batchWindow, currentTime)
            time = SimTime(t)
          } yield time

        }.toSet

        // split out invalid and valid instructions
        val (invalidInstructions, validInstructions) = batchingInstructions.partition{i => invalidTimes.contains(i.batchingTime)}

        // report invalid instructions
        for { i <- invalidInstructions } {
          logger.debug(s"batching strategy update at time $currentTime has invalid time ${i.batchingTime} for agent ${i.agentBatchData.request.agent}; ignoring")
        }

        // add all new and valid instructions, replacinging any old ones
        this.copy(
          batchingStrategy = cleanedOldStrategy ++ validInstructions.map{i => i.agentBatchData.request.agent -> i}.toMap
        )
    }
  }

  /**
    * exports any batching instructions for the current time as batches of routing requests
    * @param currentSimTime the current time
    * @return the updated batching manager as well as any batches of routing requests to solve
    */
  def getBatchesForTime(currentSimTime: SimTime): (BM_2, List[List[Request]]) = {
    // returns our strat for currentSimTime

    // instructions for this batch
    val markForThisBatch: Map[String, BatchingInstruction] =
      this.batchingStrategy.filter{ case (_, i) => i.batchingTime == currentSimTime }

    // this batch
    val batches: List[List[Request]] =
      markForThisBatch
        .groupBy{ case (_, i) => i.batchId}
        .map{ case (_, batch) =>
          batch.values.map{_.agentBatchData.request}.toList
        }
        .toList

    //
    val updatedStrat: BatchingStrategy = this.batchingStrategy -- markForThisBatch.map{ case (agentId, _) => agentId }

    (this.copy(batchingStrategy = updatedStrat), batches)
  }
}


case class BatchingManager (
  batchWindow: SimTime,
  minBatchSize: Int,
  requestUpdateCycle: SimTime,
  batchingStrategy: Map[SimTime, List[List[AgentBatchData]]] = Map.empty,
) extends LazyLogging {

  /**
    * takes a new batch strategy produced by a [[BatchingFunction]], validates it, and
    * applies it to the stored batching strategy. guards against modifying any batches
    * scheduled for the next batching SimTime. those requests should be fixed as we are
    * simulating a real-time online algorithm, and the routing algorithm would already
    * be computing the solution for those.
    *
    * @param newBatchStrategy the update to the batching strategy
    * @param currentTime the current SimTime
    * @return the updated BatchingManager
    */
  def updateBatchData(newBatchStrategy: Option[Map[SimTime, List[List[AgentBatchData]]]], currentTime: SimTime): BatchingManager = {

    // whichever strategy we store, we want to be sure that any requests in it are not stale
    val cleanStaleReqs: Map[SimTime, List[List[AgentBatchData]]] => Map[SimTime, List[List[AgentBatchData]]] =
      BatchingManager.cleanStaleRequestsInStrategy(currentTime, this.requestUpdateCycle)

    newBatchStrategy match {
      case None =>

        // no new strategy, stick with old one
        this.copy(batchingStrategy = cleanStaleReqs(this.batchingStrategy))

      case Some(newStrat) =>
        if (newStrat.isEmpty) {

          // new strategy is empty, stick with old one
          this.copy(batchingStrategy = cleanStaleReqs(this.batchingStrategy))
        } else {

          // we will want to add the Requests for the next batch time
          // to whatever update we perform
          val soonestBatchWindowStrat: Map[SimTime, List[List[AgentBatchData]]] =
            BatchingManager.retainStrategyForNearestBatchTime(
              this.batchingStrategy,
              currentTime,
              this.batchWindow
            )

          logger.debug(s"updating batch data at time $currentTime")
          BatchingManager.listInvalidStrategies(newStrat, batchWindow, currentTime) match {
            case Nil =>

              // new strategy is fine, add it to any plans which cannot be removed/changed

              this.copy(batchingStrategy = soonestBatchWindowStrat ++ cleanStaleReqs(newStrat))
            case testInvalid =>

              // new strategy has tried to update some SimTimes that are too soon
              // remove these and use the remaining additions
              val stratWithoutBadTimes: Map[SimTime, List[List[AgentBatchData]]] =
                testInvalid.foldLeft(newStrat) { (strat, badTime) =>
                  logger.debug(s"batching strategy update at time $currentTime has invalid time $badTime with ${newStrat.getOrElse(badTime, Map.empty).size} agents; ignoring")
                  strat - badTime
                }
              this.copy(batchingStrategy = cleanStaleReqs(stratWithoutBadTimes))
          }
        }
    }
  }


  /**
    * gets any batches for the current sim time. the batches should be valid, but, we first want
    * to deconstruct batches that are smaller than the minBatchSize, so they are handled with
    * a selfish routing strategy
    * @param currentSimTime the current time, which may or may not correspond with a batch
    * @return the updated batch manager along with any batches for this SimTime
    */
  def getBatchesForTime(currentSimTime: SimTime): (BatchingManager, List[List[Request]]) = {
    // grab the requests for this time, and unpack the AgentBatchData to get it's Request
    val thisTimeBatch: List[List[Request]] = batchingStrategy.get(currentSimTime).map{_.map{_.map{_.request}}}.getOrElse(List.empty)

    if (thisTimeBatch.isEmpty) (this, List.empty)
    else {

      // if a batch is smaller than the minBatchSize, then break it up into singleton lists
      // so that they are handled as selfish route requests
      val (smallBatches, largeBatches) = thisTimeBatch.partition(_.size < minBatchSize)
      val nextBatch: List[List[Request]] = largeBatches ++ smallBatches.flatMap{reqs => reqs.map{req => List(req)}}

      // remove batches from the BatchingManager related to the currentSimTime
      val updatedBatchingManager = this.copy(batchingStrategy = batchingStrategy - currentSimTime)

      logger.info(s"got ${nextBatch.size} batches for time $currentSimTime")
      logger.info(s"batch groupings:\n ${nextBatch.map{ _.size }.mkString("[", ",", "]")}")

      (updatedBatchingManager, nextBatch)
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
      .flatMap{_.responses}
      .groupBy{_.request.agent}
      .map{ case (_, solutionsForAgent: List[Response]) => solutionsForAgent.minBy{_.costEstimate} }
      .toList
}


object BatchingManager {

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
    currentTime: SimTime): Seq[SimTime] = {
    for {
      t <- currentTime until nextValidBatchingTime(batchWindow, currentTime)
      time = SimTime(t)
      if newStrategy.isDefinedAt(time)
    } yield {
      time
    }
  }

  /**
    * filter predicate which remove agents who have met the
    * minimum replanning wait time since their last replanning
    * @param currentTime
    * @param minimumReplanningWaitTime
    * @return
    */
  def filterNewDataByMinReplanningWaitTime(
    currentTime: SimTime,
    minimumReplanningWaitTime: SimTime
  ): AgentBatchData => Boolean = {
    data =>
      data.lastReplanningTime match {
        case None => true
        case Some(lastReplanningTime) =>
          lastReplanningTime + minimumReplanningWaitTime > currentTime
      }
  }

  /**
    * when we get an update, we may want to toss any old data for any agent
    * @param oldData
    * @param newData
    * @return
    */
  def keepLatestAgentBatchData(
    oldData: List[AgentBatchData],
    newData: List[AgentBatchData]
  ): List[AgentBatchData] = {
    for {
      (_, agentData) <- (oldData ++ newData).groupBy{_.request.agent}
    } yield {
      if (agentData.lengthCompare(0) > 0) {
        agentData.maxBy{_.timeOfRequest}(Ordering.by{_.value})
      } else {
        agentData.head
      }
    }
  }.toList

  /**
    * removes any stale requests using a filter
    *
    * @param strat the strat to clean
    * @param currentTime the current sim time
    * @param requestUpdateCycle the request update cycle
    * @return the cleaned stategy
    */
  def cleanStaleRequestsInStrategy(
    currentTime: SimTime,
    requestUpdateCycle: SimTime
  )(
    strat: Map[SimTime, List[List[AgentBatchData]]]
  ): Map[SimTime, List[List[AgentBatchData]]] =
    strat.flatMap{ case (simTime, batches) =>
      batches.map{
        _.filter{ data =>
          val mostRecentRequestUpdate: SimTime = currentTime - (currentTime % requestUpdateCycle)
          mostRecentRequestUpdate < data.timeOfRequest
        }
      } match {
        case Nil => None
        case filteredData => Some { simTime -> filteredData }
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
    oldStrat.filter{ case(simTime, _) => simTime == nearestBatchTime }
  }

  /**
    * removes stale data from the set of instructions
    *
    * @param currentTime the current time
    * @param requestUpdateCycle the cycle that updates are updated
    * @param instruction an instruction to test
    * @return
    */
  def isNotStaleRequest(
    currentTime: SimTime,
    requestUpdateCycle: SimTime)(
    instruction: BatchingInstruction): Boolean = {

    val mostRecentRequestUpdate: SimTime = currentTime - (currentTime % requestUpdateCycle)
    mostRecentRequestUpdate < instruction.agentBatchData.timeOfRequest
  }
}