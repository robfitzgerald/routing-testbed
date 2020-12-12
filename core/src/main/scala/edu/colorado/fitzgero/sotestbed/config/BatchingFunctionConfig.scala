package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.algorithm.batching.BatchTagger
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

sealed trait BatchingFunctionConfig {
  def build(): batching.BatchingFunction
}

object BatchingFunctionConfig {

  final case class Greedy(
    batchWindow: SimTime,
    maxBatchSize: Int
  ) extends BatchingFunctionConfig {
    def build(): batching.BatchingFunction = batching.GreedyBatching(batchWindow, maxBatchSize)
  }

  final case class GreedyCoordinateGrouping(
    batchWindow: SimTime,
    maxBatchSize: Int,
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    splitFactor: Int,
    batchPathTimeDelay: SimTime,
    batchType: String // "o", "d", "od", "c", "cd"
  ) extends BatchingFunctionConfig {
    require(splitFactor > 0, "split factor must be positive")
    require(
      BatchTagger.ValidBatchTags.contains(batchType),
      s"invalid batching-function.batch-type '$batchType': must be one of ${BatchTagger.ValidBatchTags.mkString("{", "|", "}")}"
    )

    def build(): batching.BatchingFunction =
      batching.GreedyCoordinateGridBatching(
        maxBatchSize,
        minX,
        maxX,
        minY,
        maxY,
        splitFactor,
        batchType
      ) match {
        case Left(value) =>
          throw value
        case Right(value) =>
          value
      }
  }
}
