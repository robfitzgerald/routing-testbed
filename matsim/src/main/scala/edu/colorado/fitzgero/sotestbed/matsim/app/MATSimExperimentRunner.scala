package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.Files

import scala.util.Try
import scala.util.matching.Regex

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{
  RoutingAlgorithm,
  SelfishSyncRoutingBPR,
  TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm,
  TwoPhaseRoutingAlgorithm
}
import edu.colorado.fitzgero.sotestbed.config.RoutingReportConfig.{AggregateData, BatchLearning, CompletePath, Inactive}
import edu.colorado.fitzgero.sotestbed.config.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection, TspSelection}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports
import edu.colorado.fitzgero.sotestbed.util.SummaryStats

case class MATSimExperimentRunner(matsimRunConfig: MATSimRunConfig, seed: Long) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    * @return either a matsim config setup error, or,
    *         the final state of the simulation after the simulation ends (which could be a failure as well)
    */
  def run(): Either[io.Serializable, Any] = {
    for {
      network            <- LocalAdjacencyListFlowNetwork.fromMATSimXML(matsimRunConfig.io.matsimNetworkFile, matsimRunConfig.routing.batchWindow)
      agentsUnderControl <- PopulationOps.loadAgentsUnderControl(matsimRunConfig.io.populationFile)
      config = matsimRunConfig.copy(agentsUnderControl = agentsUnderControl)
    } yield {

      // wrap config along with MATSim-specific environment values
//      val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(agentsUnderControl, config)

      Files.createDirectories(config.experimentDirectory)

      // used by reporting logic
      val costFunction: EdgeBPR => Cost =
        config.algorithm match {
          case _ @MATSimConfig.Algorithm.Selfish(_) =>
            _: EdgeBPR =>
              Cost.Zero
          case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
            val marginalCostFn: EdgeBPR => Flow => Cost = systemOptimal.marginalCostFunction.build()
            edgeBPR: EdgeBPR =>
              marginalCostFn(edgeBPR)(Flow.Zero)
        }

      // how we read reports
      val routingReportFile: File = new File(config.experimentLoggingDirectory.resolve(s"route-${config.algorithm.name}.csv").toString)
      val routingReporter: RoutingReports[SyncIO, Coordinate, EdgeBPR] =
        config.io.routingReportConfig match {
          case AggregateData => AggregateData.build(routingReportFile, costFunction)
          case CompletePath  => CompletePath.build(routingReportFile, costFunction)
          case BatchLearning => BatchLearning.build(routingReportFile)
          case Inactive      => Inactive.build()
        }

      // the actual Simulation runner instance
      val experiment = new LocalMATSimRoutingExperiment(
        new File(config.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString),
        routingReporter
      )

      val soRoutingAlgorithm: RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR] = config.algorithm match {
        case selfish @ MATSimConfig.Algorithm.Selfish(_) =>
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
          case _: MATSimConfig.Routing.Selfish.MATSim => None
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
          case selfish @ MATSimConfig.Algorithm.Selfish(edgeUpdateFunction) =>
            // need a no-batching manager version here? or, a dummy for now?
            experiment.run(
              config = config,
              roadNetwork = network,
              ueRoutingAlgorithm = ueRoutingAlgorithm,
              soRoutingAlgorithm = selfish.build(),
              updateFunction = edgeUpdateFunction.build(),
              batchingFunction = selfish.batchingStub,
              batchWindow = config.routing.batchWindow,
              minBatchSize = config.routing.minBatchSize,
              minRequestUpdateThreshold = config.routing.minRequestUpdateThreshold,
              minNetworkUpdateThreshold = config.routing.minNetworkUpdateThreshold,
              doneRoutingAtSimTime = config.run.endOfRoutingTime,
              selfishOnly = selfish.selfishOnly
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
              selfishOnly = systemOptimal.selfishOnly
            )

        }

      val result: experiment.ExperimentState = experimentSyncIO.unsafeRunSync()

      result.error.foreach(e => logger.error(e))

      // try to compute fairness
      val summaryStats: String = MATSimExperimentRunnerOps.fairness(config) match {
        case Left(error) =>
          logger.error(s"${error.getClass} ${error.getMessage} ${error.getCause}")
          ""
        case Right(stats) =>
          stats
      }

      // let's drop some knowledge at output
      val tripDurationsFile: File =
        config.experimentDirectory.resolve("ITERS").resolve("it.0").resolve("0.tripdurations.txt").toFile
      val tripDurationsSource = scala.io.Source.fromFile(tripDurationsFile)
      val avgDurRegex: Regex  = "average trip duration: (\\d+\\.\\d+) seconds".r.unanchored
      val avgDuration: String = tripDurationsSource.getLines.mkString("") match {
        case avgDurRegex(g1) => g1
        case _               => ""
      }
      tripDurationsSource.close()

      val traveldistancestatsFile: File =
        config.experimentDirectory.resolve("traveldistancestats.txt").toFile
      val travelDistancesSource = scala.io.Source.fromFile(traveldistancestatsFile)
      val avgDistRegex: Regex   = "(\\d+\\.\\d+)".r.unanchored
      val avgDistance: String = travelDistancesSource.getLines.mkString("") match {
        case avgDistRegex(g1) => g1
        case _                => ""
      }

      val batchOverviewFile: File          = config.io.batchLoggingDirectory.resolve("result.csv").toFile
      val APPEND_MODE: Boolean             = true
      val batchOverviewOutput: PrintWriter = new PrintWriter(new FileOutputStream(batchOverviewFile, APPEND_MODE))
      Try {
        val parameterColumns: String       = config.scenarioData.toCSVRow
        val (avgDistMeters, avgDurSeconds) = (avgDistance.toDouble, avgDuration.toDouble)
        val avgSpeedMph: Double            = (avgDistMeters / 1609.0) / (avgDurSeconds / 3600.0)
        batchOverviewOutput.append(s"$parameterColumns,$avgDuration,$avgDistance,$avgSpeedMph,$summaryStats\n")
      }
      batchOverviewOutput.close()
      "done"
    }
  }
}
