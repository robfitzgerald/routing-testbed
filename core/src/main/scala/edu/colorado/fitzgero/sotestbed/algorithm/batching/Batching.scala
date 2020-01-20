package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

object Batching {
  /**
    * tracks the plan for each agent in the system. limits number of batching instructions
    * for each agent to at most one.
    *
    * the [[BatchingInstruction]] tracks which batch time and which sub-batch this agent
    * belongs to. the underlying AgentBatchData is continually refreshed by the request updates.
    */
  type BatchingStrategy = Map[String, BatchingInstruction]

  final case class BatchingInstruction(
    agentBatchData: AgentBatchData,
    batchingTime: SimTime,
    batchId: String
  )
}
