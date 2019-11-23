package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

trait BatchingFunction {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    * @param currentBatchStrategy the current strategy
    * @param newBatchData some new data about agents eligible for replanning from the system
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy(currentBatchStrategy: Map[SimTime, Map[String, BatchingManager.AgentBatchData]],
                             newBatchData: List[BatchingManager.AgentBatchData],
                             currentTime: SimTime): Option[Map[SimTime, Map[String, BatchingManager.AgentBatchData]]]

}