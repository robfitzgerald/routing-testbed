package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

case class GreedyBatching (
  batchWindow: SimTime,
  minimumReplanningWaitTime: SimTime
) extends BatchingFunction {

  /**
    * simply assigns any agents eligible for planning/replanning to the next possible batch
    *
    * @param currentBatchStrategy the current strategy
    * @param newBatchData some new data about agents eligible for replanning from the system
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(currentBatchStrategy: Map[SimTime, Map[String, BatchingManager.AgentBatchData]],
    newBatchData: List[BatchingManager.AgentBatchData],
    currentTime: SimTime): Option[Map[SimTime, Map[String, BatchingManager.AgentBatchData]]] = {

    // just insert requests at the soonest possible batch time
    val nextBatchingTime: SimTime = BatchingManager.nextValidBatchingTime(batchWindow, currentTime)

    // remove agents who have exceeded the config parameter for minimum replanning wait time
    newBatchData.filter{_.lastReplanningTime + minimumReplanningWaitTime > currentTime} match {
      case Nil => None
      case newRequests =>
        // we have agents that we can replan to add to the nearest possible request time
        val agentsToAdd = newRequests.map{agentBatchData => agentBatchData.request.agent -> agentBatchData.request}.toMap
        val updatedGreedyBatchTime = currentBatchStrategy.getOrElse(nextBatchingTime, Map.empty) ++ agentsToAdd
        Some {
          currentBatchStrategy.updated(nextBatchingTime, updatedGreedyBatchTime)
        }
    }
  }
}