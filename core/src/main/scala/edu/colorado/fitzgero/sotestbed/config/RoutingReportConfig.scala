package edu.colorado.fitzgero.sotestbed.config

import java.nio.file.Path

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports._

sealed trait RoutingReportConfig

object RoutingReportConfig {
  final case object Inactive extends RoutingReportConfig {

    def build(): RoutingReports[SyncIO, Coordinate, EdgeBPR] = new NoRoutingReporter()
  }

  final case object CompletePath extends RoutingReportConfig {

    def build(outputDirectory: Path, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] = {
      val completePathFilePath: Path = outputDirectory.resolve("completePath.csv")
      new CompletePathAlternativesRoutingReport(completePathFilePath.toFile, costFunction)
    }
  }
  final case object AggregateData extends RoutingReportConfig {

    def build(outputDirectory: Path, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] = {
      val aggregateDataFilePath: Path = outputDirectory.resolve("aggregateData.csv")
      new AggregateAgentDataRoutingReport(aggregateDataFilePath.toFile, costFunction)
    }
  }

  final case object Batch extends RoutingReportConfig {

    def build(outputDirectory: Path): RoutingReports[SyncIO, Coordinate, EdgeBPR] = {
      val batchLearningFilePath: Path = outputDirectory.resolve("batchLearning.csv")
      new BatchReporter(batchLearningFilePath.toFile)
    }
  }

  final case object Heatmap extends RoutingReportConfig {

    def build(outputDirectory: Path,
              logCycle: SimTime,
              h3Resolution: Int,
              network: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
              costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] = {
      AvgSpeedHeatmapReport(outputDirectory, logCycle, h3Resolution, network, costFunction)
    }
  }

  final case object AllReporting extends RoutingReportConfig with LazyLogging {

    def build(
      outputDirectory: Path,
      logCycle: SimTime,
      h3Resolution: Int,
      network: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
      costFunction: EdgeBPR => Cost
    ): RoutingReports[SyncIO, Coordinate, EdgeBPR] = new RoutingReports[SyncIO, Coordinate, EdgeBPR] {

      val reporters: List[RoutingReports[SyncIO, Coordinate, EdgeBPR]] = List(
        CompletePath.build(outputDirectory, costFunction),
        AggregateData.build(outputDirectory, costFunction),
        Batch.build(outputDirectory),
        Heatmap.build(outputDirectory, logCycle, h3Resolution, network, costFunction)
      )

      def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                        roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                        currentTime: SimTime): SyncIO[Unit] = SyncIO {
        logger.debug(s"begin updating reports at time $currentTime")
        for {
          reporter <- reporters
        } {
          reporter.updateReports(routingResult, roadNetwork, currentTime).unsafeRunSync()
          logger.debug(s"update reporter ${reporter.getClass.getName}")
        }
        logger.debug(s"finished updating reports at time $currentTime")
      }

      def close(): Unit = for { reporter <- reporters } { reporter.close() }
    }
  }
}
