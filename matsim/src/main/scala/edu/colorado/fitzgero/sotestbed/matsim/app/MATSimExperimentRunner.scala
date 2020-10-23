package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.Files

import scala.util.Try

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{
  RoutingAlgorithm,
  SelfishSyncRoutingBPR,
  TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm,
  TwoPhaseRoutingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.config.RoutingReportConfig
import edu.colorado.fitzgero.sotestbed.config.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection, TspSelection}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{AgentBaseMetrics, AgentPerformanceMetrics}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

case class MATSimExperimentRunner(matsimRunConfig: MATSimRunConfig, seed: Long) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    * @return either a matsim config setup error, or,
    *         the final state of the simulation after the simulation ends (which could be a failure as well)
    */
  def run(): Either[io.Serializable, Any] = {
    for {
      network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimRunConfig.io.matsimNetworkFile, matsimRunConfig.routing.batchWindow)
      agentsUnderControlPercentage = if (matsimRunConfig.algorithm.isInstanceOf[MATSimConfig.Algorithm.Selfish]) 0.0
      else matsimRunConfig.routing.adoptionRate
      agentsUnderControl <- PopulationOps.loadAgentsUnderControl(matsimRunConfig.io.populationFile, agentsUnderControlPercentage)
      config = matsimRunConfig.copy(agentsUnderControl = agentsUnderControl)
    } yield {

      Files.createDirectories(config.experimentDirectory)

      // used by reporting logic
      val edgeFlowCostFunction: EdgeBPR => Cost = {
        val marginalCostFn: EdgeBPR => Flow => Cost = config.algorithm.marginalCostFunction.build()
        edgeBPR: EdgeBPR =>
          marginalCostFn(edgeBPR)(Flow.Zero)
      }

      // how we read reports
      val routingReporter: RoutingReports[SyncIO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case RoutingReportConfig.Inactive =>
            RoutingReportConfig.Inactive.build()
          case RoutingReportConfig.AggregateData =>
            RoutingReportConfig.AggregateData.build(config.experimentLoggingDirectory, edgeFlowCostFunction)
          case RoutingReportConfig.Batch =>
            RoutingReportConfig.Batch.build(config.experimentLoggingDirectory)
          case RoutingReportConfig.CompletePath =>
            RoutingReportConfig.CompletePath.build(config.experimentLoggingDirectory, edgeFlowCostFunction)
          case RoutingReportConfig.Heatmap =>
            RoutingReportConfig.Heatmap.build(config.experimentLoggingDirectory,
                                              SimTime.minute(config.io.heatmapLogCycleMinutes),
                                              config.io.heatmapH3Resolution,
                                              network,
                                              edgeFlowCostFunction)
          case RoutingReportConfig.AllReporting =>
            RoutingReportConfig.AllReporting.build(config.experimentLoggingDirectory,
                                                   SimTime.minute(config.io.heatmapLogCycleMinutes),
                                                   config.io.heatmapH3Resolution,
                                                   network,
                                                   edgeFlowCostFunction)
        }

      // the actual Simulation runner instance
      val experiment = new LocalMATSimRoutingExperiment(
        new File(config.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString),
        routingReporter
      )

      val soRoutingAlgorithm: RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR] = config.algorithm match {
        case selfish @ MATSimConfig.Algorithm.Selfish(_, _) =>
          // need a no-phase dijkstra's algorithm here?
          selfish.build()
        case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
          systemOptimal.selectionAlgorithm match {
            case local: LocalMCTSSelection =>
              // special effect handling for MCTS library
              new TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm[Coordinate](
                altPathsAlgorithm = systemOptimal.kspAlgorithm.build(),
                selectionAlgorithm = local.build(),
                pathToMarginalFlowsFunction = systemOptimal.pathToMarginalFlowsFunction.build(),
                combineFlowsFunction = systemOptimal.combineFlowsFunction.build(),
                marginalCostFunction = systemOptimal.marginalCostFunction.build(),
                useFreeFlowNetworkCostsInPathSearch = systemOptimal.useFreeFlowNetworkCostsInPathSearch,
                minimumAverageImprovement = config.routing.minimumAverageImprovement,
                minBatchSize = config.routing.minBatchSize,
                kspFilterFunction = systemOptimal.kspFilterFunction.build(),
                seed = seed
              )
            case rand: RandomSamplingSelection =>
              // other libraries play well
              new TwoPhaseRoutingAlgorithm[SyncIO, Coordinate, EdgeBPR](
                altPathsAlgorithm = systemOptimal.kspAlgorithm.build(),
                selectionAlgorithm = rand.build(),
                pathToMarginalFlowsFunction = systemOptimal.pathToMarginalFlowsFunction.build(),
                combineFlowsFunction = systemOptimal.combineFlowsFunction.build(),
                marginalCostFunction = systemOptimal.marginalCostFunction.build()
              )
            case TspSelection =>
              // this enables the scenario where UE agents only get one plan, and SO agents receive multiple
              // selfish routes throughout their day
              new TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm[Coordinate](
                altPathsAlgorithm = systemOptimal.kspAlgorithm.build(),
                selectionAlgorithm = TspSelection.build(),
                pathToMarginalFlowsFunction = systemOptimal.pathToMarginalFlowsFunction.build(),
                combineFlowsFunction = systemOptimal.combineFlowsFunction.build(),
                marginalCostFunction = systemOptimal.marginalCostFunction.build(),
                useFreeFlowNetworkCostsInPathSearch = systemOptimal.useFreeFlowNetworkCostsInPathSearch,
                minimumAverageImprovement = config.routing.minimumAverageImprovement,
                minBatchSize = config.routing.minBatchSize,
                kspFilterFunction = systemOptimal.kspFilterFunction.build(),
                seed = seed
              )
          }
      }

      val ueRoutingAlgorithm: Option[RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR]] =
        config.routing.selfish match {
          case _: MATSimConfig.Routing.Selfish.MATSim =>
            None
          case MATSimConfig.Routing.Selfish.Dijkstra(pathToMarginalFlowsFunction, combineFlowsFunction, marginalCostFunction) =>
            Some {
              SelfishSyncRoutingBPR(
                marginalCostFunction.build(),
                pathToMarginalFlowsFunction.build(),
                combineFlowsFunction.build()
              )
            }
        }

      val experimentSyncIO: SyncIO[experiment.ExperimentState] =
        config.algorithm match {
          case selfish @ MATSimConfig.Algorithm.Selfish(edgeUpdateFunction, _) =>
            // need a no-batching manager version here? or, a dummy for now?
            experiment.run(
              config = config,
              roadNetwork = network,
              ueRoutingAlgorithm = ueRoutingAlgorithm,
              soRoutingAlgorithm = soRoutingAlgorithm,
              updateFunction = edgeUpdateFunction.build(),
              batchingFunction = selfish.batchingStub,
              batchWindow = config.routing.batchWindow,
              minBatchSize = config.routing.minBatchSize,
              minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold,
              minNetworkUpdateThreshold = config.routing.minNetworkUpdateThreshold,
              doneRoutingAtSimTime = config.run.endOfRoutingTime,
            )
          case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
            experiment.run(
              config = config,
              roadNetwork = network,
              ueRoutingAlgorithm = ueRoutingAlgorithm,
              soRoutingAlgorithm = soRoutingAlgorithm,
              updateFunction = systemOptimal.edgeUpdateFunction.build(), // <- comes from same source that will feed routingAlgorithm above
              batchingFunction = systemOptimal.batchingFunction.build(),
              batchWindow = config.routing.batchWindow,
              minBatchSize = config.routing.minBatchSize,
              minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold,
              minNetworkUpdateThreshold = config.routing.minNetworkUpdateThreshold,
              doneRoutingAtSimTime = config.run.endOfRoutingTime,
            )
        }

      val result: experiment.ExperimentState = experimentSyncIO.unsafeRunSync()
      experiment.close()

      result.error.foreach(e => logger.error(e))

      // try to compute summary statistics from agentExperience files
      val performanceMetricsResult = for {
        overallMetrics     <- AgentBaseMetrics(config)
        performanceMetrics <- AgentPerformanceMetrics.fromConfig(config)
        batchOverviewFile = config.io.batchLoggingDirectory.resolve("result.csv").toFile
        appendMode        = true
        batchOverviewOutput <- Try { new PrintWriter(new FileOutputStream(batchOverviewFile, appendMode)) }.toEither
      } yield {
        val parameterColumns: String = config.scenarioData.toCSVRow
        batchOverviewOutput.append(s"$parameterColumns,$overallMetrics,$performanceMetrics\n")
        batchOverviewOutput.close()
      }

      performanceMetricsResult match {
        case Left(e) =>
          logger.error(s"${e.getCause} ${e.getMessage}\n${e.getStackTrace.mkString("Array(", ", ", ")")}")
          s"${e.getCause} ${e.getMessage}\n${e.getStackTrace.mkString("Array(", ", ", ")")}"
        case Right(_) =>
          "done."
      }
    }
  }
}
