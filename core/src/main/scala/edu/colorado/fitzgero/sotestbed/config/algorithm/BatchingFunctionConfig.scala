package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

sealed trait BatchingFunctionConfig {
  def build(): batching.BatchingFunction
}
object BatchingFunctionConfig {
  final case class Greedy(
    batchWindow: SimTime,
    minimumReplanningWaitTime: SimTime,
    maxBatchSize: Int
  ) extends BatchingFunctionConfig {
    def build(): batching.BatchingFunction = batching.GreedyBatching(batchWindow, minimumReplanningWaitTime, maxBatchSize)
  }

//  final case class GreedyCoordinateGrid (
//    batchWindow: SimTime,
//    minimumReplanningWaitTime: SimTime,
//    maxBatchSize: Int,
//    minX: Double,
//    maxX: Double,
//    minY: Double,
//    maxY: Double,
//    splitFactor: Int,
//    batchPathTimeDelay: SimTime
//  ) extends BatchingFunctionConfig {
//    require(splitFactor > 0, "split factor must be positive")
//    def build(): batching.BatchingFunction =
//      new batching.GreedyCoordinateGridBatching(
//        batchWindow, minimumReplanningWaitTime, maxBatchSize, minX, maxX, minY, maxY, splitFactor, batchPathTimeDelay
//      )
//  }

  final case class GreedyCoordinateGrouping (
    batchWindow: SimTime,
    minimumReplanningWaitTime: SimTime,
    maxBatchSize: Int,
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    splitFactor: Int,
    batchPathTimeDelay: SimTime,
    batchType: String // "o", "d", "od"
  ) extends BatchingFunctionConfig {
    require(splitFactor > 0, "split factor must be positive")
    def build(): batching.BatchingFunction =
      new batching.GreedyCoordinateGridBatching(
        batchWindow, minimumReplanningWaitTime, maxBatchSize, minX, maxX, minY, maxY, splitFactor, batchPathTimeDelay, batchType
      )
  }
}