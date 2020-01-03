package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime


case class BatchingManager (
  batchWindow     : SimTime,
  batchingStrategy: Map[SimTime, List[List[AgentBatchData]]] = Map.empty,
) extends LazyLogging {

  /**
    * looks up any batch for this sim time (possibly empty) and return it
    * @param currentSimTime the current sim time
    * @return the updated BatchingManager along with any requests for this sim time
    */
  def getBatchesForTime(currentSimTime: SimTime): (BatchingManager, List[List[Request]]) = {
    logger.debug(s"requesting batch for time $currentSimTime")
    val nextBatch = batchingStrategy
      .get(currentSimTime)
      .map{_.map{_.map{_.request}}}
      .getOrElse(List.empty)
    val updatedBatchingManager =
      this.copy(
        batchingStrategy = batchingStrategy - currentSimTime
      )
    (updatedBatchingManager, nextBatch)
  }

  /**
    * takes a new batching strategy, and if it is valid, updates the batching manager;
    * if invalid (a batch assignment occurs for a time sooner than could properly be handled
    * in simulation time), then no update occurs and the error is logged (fails silently).
    * @param newBatchStrategy a new batching strategy from a BatchingFunction
    * @param currentTime the current sim time
    * @return the updated batching manager (or the same one)
    */
  def updateBatchData(newBatchStrategy: Option[Map[SimTime, List[List[AgentBatchData]]]], currentTime: SimTime): BatchingManager = {
    logger.debug(s"updating batch data at time $currentTime")
    newBatchStrategy match {
      case None => this
      case Some(newStrat) =>
        BatchingManager.listInvalidStrategies(newStrat, batchWindow, currentTime) match {
          case Nil =>
            this.copy(batchingStrategy=newStrat)
          case testInvalid =>
            testInvalid.foreach{ time =>
              logger.debug(s"batching strategy update at time $currentTime has invalid time $time with ${newStrat.getOrElse(time, Map.empty).size} agents; ignoring")
            }
            this
        }
    }
  }
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
  def listInvalidStrategies(
    newStrategy: Map[SimTime, List[List[AgentBatchData]]],
    batchWindow: SimTime,
    currentTime: SimTime): Seq[SimTime] = {
    for {
      t   <- currentTime until nextValidBatchingTime(batchWindow, currentTime)
      time = SimTime(t)
      if newStrategy.isDefinedAt(time)
    } yield {
      time
    }
  }
}