package edu.colorado.fitzgero.sotestbed.config

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.batching
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{
  AgentBatchData,
  BatchingFunction,
  LBTCTrajectoryClusterBatching
}
import edu.colorado.fitzgero.sotestbed.algorithm.grid.{BatchTagger, CoordinateGrid2}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkZone
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import com.typesafe.scalalogging.LazyLogging
import java.nio.file.Path
import cats.effect.unsafe.implicits.global

sealed trait BatchingFunctionConfig {
  def build(coordinateGrid2: CoordinateGrid2): batching.BatchingFunction
}

object BatchingFunctionConfig extends LazyLogging {

  case object NoBatching extends BatchingFunctionConfig {

    def build(coordinateGrid2: CoordinateGrid2): BatchingFunction =
      new BatchingFunction {

        def updateBatchingStrategy(
          roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
          activeRouteRequests: List[AgentBatchData.RouteRequestData],
          currentTime: SimTime
        ): IO[Option[BatchingFunction.BatchingResult]] = {
          if (activeRouteRequests.isEmpty) IO(None)
          else {
            val singleBatch: List[(String, List[Request])] = List(("all", activeRouteRequests.map { _.request }))
            val dummyLookup                                = Map.empty[String, List[EdgeId]]
            IO(Some(BatchingFunction.BatchingResult(singleBatch, dummyLookup)))
          }
        }
      }
  }

  case class Random(
    batchWindow: SimTime,
    maxBatchSize: Int
  ) extends BatchingFunctionConfig {

    override def build(coordinateGrid2: CoordinateGrid2): batching.BatchingFunction =
      batching.RandomBatching(batchWindow, maxBatchSize)
  }

  final case class Greedy(
    maxBatchSize: Int,
    maxBatchRadius: Double
  ) extends BatchingFunctionConfig {

    override def build(coordinateGrid2: CoordinateGrid2): BatchingFunction =
      batching.GreedyBatching(maxBatchSize, maxBatchRadius)
  }

  case class CoordinateGridGrouping(
    maxBatchSize: Int,
    batchType: String // "o", "d", "od", "c", "cd"
  ) extends BatchingFunctionConfig {
    require(
      BatchTagger.ValidBatchTags.contains(batchType),
      s"invalid batching-function.batch-type '$batchType': must be one of ${BatchTagger.ValidBatchTags.mkString("{", "|", "}")}"
    )

    override def build(coordinateGrid2: CoordinateGrid2): batching.BatchingFunction =
      batching.CoordinateGridBatching(coordinateGrid2, batchType, maxBatchSize) match {
        case Left(value) =>
          throw value
        case Right(value) =>
          value
      }
  }

  case class LabelBasedTrajectoryClustering(
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

    override def build(coordinateGrid2: CoordinateGrid2): BatchingFunction = LBTCTrajectoryClusterBatching(
      omegaDelta,
      omegaBeta,
      omegaA,
      omegaS,
      maxIterations,
      maxRuntimeMilliseconds.getOrElse(DefaultMaxRuntimeMilliseconds),
      trajectoryTimeLimit
    )
  }

  case class NetworkZoneBatching(zonesFile: Option[Path]) extends BatchingFunctionConfig {

    override def build(coordinateGrid2: CoordinateGrid2): batching.BatchingFunction = {
      zonesFile match {
        case Some(zoneFilePath) =>
          logger.info(s"user provided batching file ${zoneFilePath}")
          batching.NetworkZoneBatching.fromFile(zoneFilePath).unsafeRunSync
        case None =>
          logger.info(s"user provided grid file with ${coordinateGrid2.gridCells.size} grid cells as batching zones")
          batching.NetworkZoneBatching(coordinateGrid2)
      }

    }

  }
}
