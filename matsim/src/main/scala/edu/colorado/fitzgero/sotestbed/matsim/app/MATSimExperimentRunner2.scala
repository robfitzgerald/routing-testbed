package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.Files

import scala.util.Try

import cats.Id
import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm.AltPathsResult
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{
  RoutingAlgorithm,
  RoutingAlgorithm2,
  SelfishSyncRoutingBPR,
  TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm,
  TwoPhaseRoutingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionRunner}
import edu.colorado.fitzgero.sotestbed.config.{RoutingReportConfig, SelectionAlgorithmConfig}
import edu.colorado.fitzgero.sotestbed.config.SelectionAlgorithmConfig.{
  LocalMCTSSelection,
  RandomSamplingSelection,
  TspSelection
}
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{AgentBaseMetrics, AgentPerformanceMetrics}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.{LocalMATSimRoutingExperiment, LocalMATSimRoutingExperiment2}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.{
  toDoubleWithMinimum,
  Coordinate
}
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

case class MATSimExperimentRunner2(matsimRunConfig: MATSimRunConfig, seed: Long) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    * @return either a matsim config setup error, or,
    *         the final state of the simulation after the simulation ends (which could be a failure as well)
    */
  def run(): Either[Exception, String] = {
    val result = for {
      network <- LocalAdjacencyListFlowNetwork
        .fromMATSimXML(
          matsimRunConfig.io.matsimNetworkFile,
          matsimRunConfig.routing.batchWindow
        )
        .left
        .map { s => new Error(s) }
      agentsUnderControlPercentage = if (matsimRunConfig.algorithm.isInstanceOf[MATSimConfig.Algorithm.Selfish]) 0.0
      else matsimRunConfig.routing.adoptionRate
      agentsUnderControl <- PopulationOps.loadAgentsUnderControl(
        matsimRunConfig.io.populationFile,
        agentsUnderControlPercentage
      )
      config = matsimRunConfig.copy(agentsUnderControl = agentsUnderControl)
    } yield {

      Files.createDirectories(config.experimentDirectory)

      // build some cost functions
      val freeFlowCostFunction: EdgeBPR => Cost = (edgeBPR: EdgeBPR) => edgeBPR.freeFlowCost
      val nonMarginalCostFunction: EdgeBPR => Cost = {
        val marginalCostFn: EdgeBPR => Flow => Cost = config.algorithm.marginalCostFunction.build()
        edgeBPR: EdgeBPR => marginalCostFn(edgeBPR)(Flow.Zero)
      }

      // how we read reports
      val routingReporter: RoutingReports[IO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case RoutingReportConfig.Inactive =>
            RoutingReportConfig.Inactive.build()
          case RoutingReportConfig.AggregateData =>
            RoutingReportConfig.AggregateData.build(config.experimentLoggingDirectory, nonMarginalCostFunction)
          case RoutingReportConfig.Batch =>
            RoutingReportConfig.Batch.build(config.experimentLoggingDirectory)
          case RoutingReportConfig.CompletePath =>
            RoutingReportConfig.CompletePath.build(config.experimentLoggingDirectory, nonMarginalCostFunction)
          case RoutingReportConfig.Heatmap =>
            RoutingReportConfig.Heatmap.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              nonMarginalCostFunction
            )
          case RoutingReportConfig.AllAggregate =>
            RoutingReportConfig.AllAggregate.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              nonMarginalCostFunction
            )
          case RoutingReportConfig.AllReporting =>
            RoutingReportConfig.AllReporting.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              nonMarginalCostFunction
            )
        }

      // the actual Simulation runner instance
      val experiment = new LocalMATSimRoutingExperiment2(
        new File(config.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString),
        routingReporter
      )

      val ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, Coordinate, EdgeBPR]] =
        config.routing.selfish match {
          case _: MATSimConfig.Routing.Selfish.MATSim =>
            None
          case MATSimConfig.Routing.Selfish
                .Dijkstra(pathToMarginalFlowsFunction, combineFlowsFunction, marginalCostFunction) =>
            Some {
              SelfishSyncRoutingBPR(
                marginalCostFunction.build(),
                pathToMarginalFlowsFunction.build(),
                combineFlowsFunction.build()
              )
            }
        }

      val soRoutingAlgorithm: Option[RoutingAlgorithm2[Coordinate]] =
        config.algorithm match {
          case so: Algorithm.SystemOptimal =>
            val ksp: AltPathsAlgorithmRunner[IO, Coordinate, EdgeBPR] = {
              AltPathsAlgorithmRunner(
                altPathsAlgorithm = so.kspAlgorithm.build(),
                kspFilterFunction = so.kspFilterFunction.build(),
                costFunction = nonMarginalCostFunction,
                freeFlowCostFunction = freeFlowCostFunction,
                useFreeFlowNetworkCostsInPathSearch = so.useFreeFlowNetworkCostsInPathSearch,
                seed = seed
              )
            }
            val selectionAlgorithm: SelectionAlgorithm[IO, Coordinate, EdgeBPR] = so.selectionAlgorithm match {
              case rand: RandomSamplingSelection =>
                rand.build()
              case mcts: LocalMCTSSelection =>
                mcts.build()
              case SelectionAlgorithmConfig.TspSelection =>
                TspSelection.build()
            }
            val sel: SelectionRunner[Coordinate] =
              SelectionRunner(
                selectionAlgorithm = selectionAlgorithm,
                pathToMarginalFlowsFunction = so.pathToMarginalFlowsFunction.build(),
                combineFlowsFunction = so.combineFlowsFunction.build(),
                marginalCostFunction = so.marginalCostFunction.build(),
                minimumAverageImprovement = config.routing.minimumAverageImprovement
              )
            val alg = RoutingAlgorithm2(
              altPathsAlgorithmRunner = ksp,
              batchingFunction = so.batchingFunction.build(),
              batchFilterFunction = so.batchFilterFunction.build(Some(config.routing.minBatchSearchSpace)),
              selectionRunner = sel,
              k = so.kspAlgorithm.k,
              minSearchSpaceSize = config.routing.minBatchSearchSpace
            )
            Some(alg)
          case _: Algorithm.Selfish =>
            None
        }

      val experimentIO: IO[experiment.ExperimentState] =
        experiment.run(
          config = config,
          roadNetwork = network,
          ueRoutingAlgorithm = ueRoutingAlgorithm,
          soRoutingAlgorithm = soRoutingAlgorithm,
          updateFunction = config.algorithm.edgeUpdateFunction.build(),
          batchWindow = config.routing.batchWindow,
          minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold
        )

      val experimentResult = Try {
        val result: experiment.ExperimentState = experimentIO.unsafeRunSync()
        experiment.close()

        // try to compute summary statistics from agentExperience files
        for {
          overallMetrics     <- AgentBaseMetrics(config)
          performanceMetrics <- AgentPerformanceMetrics.fromConfig(config)
          batchOverviewFile = config.io.batchLoggingDirectory.resolve("result.csv").toFile
          appendMode        = true
          batchOverviewOutput <- Try {
            new PrintWriter(new FileOutputStream(batchOverviewFile, appendMode))
          }.toEither
        } yield {
          val parameterColumns: String = config.scenarioData.toCSVRow
          batchOverviewOutput.append(s"$parameterColumns,$overallMetrics,$performanceMetrics\n")
          batchOverviewOutput.close()
        }
      }.toEither match {
        case Left(e) =>
          logger.error(s"${e.getCause} ${e.getMessage}\n${e.getStackTrace.mkString("Array(", ", ", ")")}")
          s"${e.getCause} ${e.getMessage}\n${e.getStackTrace.mkString("Array(", ", ", ")")}"
        case Right(_) =>
          "done."
      }

      experimentResult
    }
    result.left.map { t =>
      new Exception(s"failure for MATSim run in directory '${matsimRunConfig.experimentDirectory}'", t)
    }
  }
}
