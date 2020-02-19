package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path}

import scala.util.Try
import scala.util.matching.Regex

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, SelfishSyncRoutingBPR, TwoPhaseLocalMCTSEdgeBPRKSPFilterRoutingAlgorithm, TwoPhaseLocalMCTSEdgeBPRRoutingAlgorithm, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.config.algorithm.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection}
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.matsim.model.roadnetwork.MATSimCapacitiesOverRoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

case class MATSimExperimentRunner(config: MATSimConfig, popSize: Int, seed: Long, trialDataOption: Option[MATSimConfig.IO.ScenarioData] = None) extends LazyLogging {

  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    * @return either a matsim config setup error, or,
    *         the final state of the simulation after the simulation ends (which could be a failure as well)
    */
  def run(): Either[io.Serializable, Any] = {
    for {
      network            <- LocalAdjacencyListFlowNetwork.fromMATSimXML(config.io.matsimNetworkFile, config.routing.batchWindow)
      agentsUnderControl <- PopulationOps.loadAgentsUnderControl(config.io.populationFile)
    } yield {

      // wrap config along with MATSim-specific environment values
      val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(agentsUnderControl, config)

      Files.createDirectories(config.io.experimentDirectory)
      val experiment = new LocalMATSimRoutingExperiment(
        new File(config.io.experimentLoggingDirectory.resolve(s"route-${config.algorithm.name}.csv").toString),
        new File(config.io.experimentLoggingDirectory.resolve(s"final-${config.algorithm.name}.log").toString)
      )

      val soRoutingAlgorithm: RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR] = matsimRunConfig.algorithm match {
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
          }
      }

      val ueRoutingAlgorithm: Option[RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR]] =
        matsimRunConfig.routing.selfish match {
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
        matsimRunConfig.algorithm match {
          case selfish @ MATSimConfig.Algorithm.Selfish(edgeUpdateFunction) =>
            // need a no-batching manager version here? or, a dummy for now?
            experiment.run(
              config = matsimRunConfig,
              roadNetwork = network,
              ueRoutingAlgorithm = ueRoutingAlgorithm,
              soRoutingAlgorithm = selfish.build(),
              updateFunction = edgeUpdateFunction.build(),
              batchingFunction = selfish.batchingStub,
              batchWindow = matsimRunConfig.routing.batchWindow,
              minBatchSize = matsimRunConfig.routing.minBatchSize,
              requestUpdateCycle = matsimRunConfig.routing.requestUpdateCycle,
              doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime,
              selfishOnly = selfish.selfishOnly
            )
          case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
            experiment.run(
              config = matsimRunConfig,
              roadNetwork = network,
              ueRoutingAlgorithm = ueRoutingAlgorithm,
              soRoutingAlgorithm = soRoutingAlgorithm,
              updateFunction = systemOptimal.edgeUpdateFunction.build(), // <- comes from same source that will feed routingAlgorithm above
              batchingFunction = systemOptimal.batchingFunction.build(),
              batchWindow = matsimRunConfig.routing.batchWindow,
              minBatchSize = matsimRunConfig.routing.minBatchSize,
              requestUpdateCycle = matsimRunConfig.routing.requestUpdateCycle,
              doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime,
              selfishOnly = systemOptimal.selfishOnly
            )

        }

      val result: experiment.ExperimentState = experimentSyncIO.unsafeRunSync()

      result.error.foreach(e => logger.error(e))

      // let's drop some knowledge at output
      val tripDurationsFile: File =
        matsimRunConfig.io.experimentDirectory.resolve("ITERS").resolve("it.0").resolve("0.tripdurations.txt").toFile
      val tripDurationsSource = scala.io.Source.fromFile(tripDurationsFile)
      val avgDurRegex: Regex = "average trip duration: (\\d+\\.\\d+) seconds".r.unanchored
      val avgDuration: String = tripDurationsSource.getLines.mkString("") match {
        case avgDurRegex(g1) => g1
        case _ => ""
      }
      tripDurationsSource.close()

      val traveldistancestatsFile: File =
        matsimRunConfig.io.experimentDirectory.resolve("traveldistancestats.txt").toFile
      val travelDistancesSource = scala.io.Source.fromFile(traveldistancestatsFile)
      val avgDistRegex: Regex = "(\\d+\\.\\d+)".r.unanchored
      val avgDistance: String = travelDistancesSource.getLines.mkString("") match {
        case avgDistRegex(g1) => g1
        case _ => ""
      }

      val batchOverviewFile: File = matsimRunConfig.io.batchLoggingDirectory.resolve("result.csv").toFile
      val APPEND_MODE: Boolean = true
      val batchOverviewOutput: PrintWriter = new PrintWriter(new FileOutputStream(batchOverviewFile, APPEND_MODE))
      val scenarioName: String = matsimRunConfig.io.scenarioData.map{_.toTrialName}.getOrElse("")
      Try {
        val (avgDistMeters, avgDurSeconds) = (avgDistance.toDouble, avgDuration.toDouble)
        val avgSpeedMph: Double = (avgDistMeters/1609.0) / (avgDurSeconds/3600.0)
        batchOverviewOutput.append(s"$scenarioName,$popSize,$avgDuration,$avgDistance,$avgSpeedMph\n")
      }
      batchOverviewOutput.close()
      "done"
    }
  }
}
