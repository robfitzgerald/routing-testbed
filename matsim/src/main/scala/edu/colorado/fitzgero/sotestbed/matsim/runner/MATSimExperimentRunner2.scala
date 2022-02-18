package edu.colorado.fitzgero.sotestbed.matsim.runner

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.Files

import scala.util.Try

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, RoutingAlgorithm2, SelfishSyncRoutingBPR}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionRunner}
import edu.colorado.fitzgero.sotestbed.config.RoutingReportConfig
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{AgentBaseMetrics, AgentPerformanceMetrics}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.Algorithm
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment2
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

case class MATSimExperimentRunner2(matsimRunConfig: MATSimRunConfig, seed: Long) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    *
    * @return either a matsim config setup error, or,
    * the final state of the simulation after the simulation ends (which could be a failure as well)
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
      val costFunction: EdgeBPR => Cost = {
        val marginalCostFn: EdgeBPR => Flow => Cost = config.algorithm.marginalCostFunction.build()
        edgeBPR: EdgeBPR => marginalCostFn(edgeBPR)(Flow.Zero)
      }

      // how we read reports
      val routingReporter: RoutingReports[IO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case RoutingReportConfig.Inactive =>
            RoutingReportConfig.Inactive.build()
          case RoutingReportConfig.AggregateData =>
            RoutingReportConfig.AggregateData.build(config.experimentLoggingDirectory, costFunction)
          case RoutingReportConfig.Batch =>
            RoutingReportConfig.Batch.build(config.experimentLoggingDirectory)
          case RoutingReportConfig.CompletePath =>
            RoutingReportConfig.CompletePath.build(config.experimentLoggingDirectory, costFunction)
          case RoutingReportConfig.Heatmap =>
            RoutingReportConfig.Heatmap.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
          case RoutingReportConfig.AllAggregate =>
            RoutingReportConfig.AllAggregate.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
          case RoutingReportConfig.AllReporting =>
            RoutingReportConfig.AllReporting.build(
              config.experimentLoggingDirectory,
              SimTime.minute(config.io.heatmapLogCycleMinutes),
              config.io.heatmapH3Resolution,
              network,
              costFunction
            )
        }

      // the actual Simulation runner instance
      val experiment = new LocalMATSimRoutingExperiment2(
        new File(config.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString),
        routingReporter
      )

      val ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, Coordinate, EdgeBPR]] =
        config.routing.selfish match {
          case _: MATSimConfig.Routing.Selfish.Matsim =>
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

      val soRoutingAlgorithmOrError: Either[Error, Option[RoutingAlgorithm2]] =
        config.algorithm match {
          case so: Algorithm.SystemOptimal =>
            val soAlgorithmOrError = for {
              grid <- so.grid.build()
            } yield {
              val ksp: AltPathsAlgorithmRunner[IO, Coordinate, EdgeBPR] = {
                AltPathsAlgorithmRunner(
                  altPathsAlgorithm = so.kspAlgorithm.build(),
                  kspFilterFunction = so.kspFilterFunction.build(),
                  costFunction = costFunction,
                  freeFlowCostFunction = freeFlowCostFunction,
                  useFreeFlowNetworkCostsInPathSearch = so.useFreeFlowNetworkCostsInPathSearch,
                  seed = seed
                )
              }
              val selectionAlgorithm: SelectionAlgorithm[IO, Coordinate, EdgeBPR] =
                so.selectionAlgorithm.build(config.experimentLoggingDirectory)

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
                batchingFunction = so.batchingFunction.build(grid),
                batchFilterFunction =
                  so.batchFilterFunction.build(Some(config.routing.minBatchSearchSpace), grid, costFunction),
                selectionRunner = sel,
                k = so.kspAlgorithm.k,
                minSearchSpaceSize = config.routing.minBatchSearchSpace
              )
              Some(alg)
            }

            soAlgorithmOrError

          case _: Algorithm.Selfish =>
            Right(None)
        }

      val experimentIO: IO[experiment.ExperimentState] = for {
        soAlgorithm <- IO.fromEither(soRoutingAlgorithmOrError)
        experimentFinishState <- experiment.run(
          config = config,
          roadNetwork = network,
          ueRoutingAlgorithm = ueRoutingAlgorithm,
          soRoutingAlgorithm = soAlgorithm,
          updateFunction = config.algorithm.edgeUpdateFunction.build(),
          batchWindow = config.routing.batchWindow,
          minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold,
          bank = Map.empty
        )
      } yield experimentFinishState

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
