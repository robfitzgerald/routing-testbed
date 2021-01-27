package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{BatchingFunction, LBTCTrajectoryClusterBatching}
import edu.colorado.fitzgero.sotestbed.algorithm.grid.BatchTagger
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

sealed trait BatchingFunctionConfig {
  def build(): batching.BatchingFunction
}

object BatchingFunctionConfig {

  final case class Random(
    batchWindow: SimTime,
    maxBatchSize: Int
  ) extends BatchingFunctionConfig {
    def build(): batching.BatchingFunction = batching.RandomBatching(batchWindow, maxBatchSize)
  }

  final case class Greedy(
    maxBatchSize: Int,
    maxBatchRadius: Double
  ) extends BatchingFunctionConfig {
    def build(): BatchingFunction = batching.GreedyBatching(maxBatchSize, maxBatchRadius)
  }

  final case class CoordinateGridGrouping(
    batchWindow: SimTime,
    maxBatchSize: Int,
    minX: Double,
    maxX: Double,
    minY: Double,
    maxY: Double,
    gridCellSideLength: Double,
    batchPathTimeDelay: SimTime,
    srid: Int,
    batchType: String // "o", "d", "od", "c", "cd"
  ) extends BatchingFunctionConfig {
    require(gridCellSideLength > 0.0, "gridCellSideLength must be positive")
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
        gridCellSideLength,
        srid,
        batchType
      ) match {
        case Left(value) =>
          throw value
        case Right(value) =>
          value
      }
  }

  final case class LabelBasedTrajectoryClustering(
    omegaDelta: Double,
    omegaBeta: Double,
    omegaA: Double,
    omegaS: Double,
    maxIterations: Int,
    maxRuntimeMilliseconds: Option[Int],
    trajectoryTimeLimit: SimTime
  ) extends BatchingFunctionConfig {

    require(omegaDelta >= 0, "omegaDelta must be in range [0, 1]")
    require(omegaBeta >= 0, "omegaBeta must be in range [0, 1]")
    require(omegaA >= 0, "omegaA must be in range [0, 1]")
    require(omegaS >= 0, "omegaS must be in range [0, 1]")
    require(omegaDelta + omegaBeta == 1, "omegaDelta + omegaBeta must equal 1")
    require(omegaA + omegaS == 1, "omegaA + omegaS must equal 1")
    require(maxIterations > 0, "maxIterations must be greater than 0")
    require(trajectoryTimeLimit > SimTime.Zero, "trajectoryTimeLimit must be greater than 0")

    val DefaultMaxRuntimeMilliseconds = 10000

    def build(): BatchingFunction = LBTCTrajectoryClusterBatching(
      omegaDelta,
      omegaBeta,
      omegaA,
      omegaS,
      maxIterations,
      maxRuntimeMilliseconds.getOrElse(DefaultMaxRuntimeMilliseconds),
      trajectoryTimeLimit
    )
  }
}
