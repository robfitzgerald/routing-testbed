package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

sealed trait BatchingFunction {
  def build(): batching.BatchingFunction
}
object BatchingFunction {
  final case class Greedy(
    batchWindow: SimTime,
    minimumReplanningWaitTime: SimTime,
    maxBatchSize: Int
  ) extends BatchingFunction {
    def build(): batching.BatchingFunction = batching.GreedyBatching(batchWindow, minimumReplanningWaitTime, maxBatchSize)
  }
}