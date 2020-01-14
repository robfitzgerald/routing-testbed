package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.File
import java.nio.file.Files

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, TwoPhaseLocalMCTSRoutingAlgorithm, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.config.algorithm.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection}
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

case class MATSimExperimentRunner (config: MATSimConfig, trialDataOption: Option[MATSimConfig.IO.ScenarioData] = None) extends LazyLogging {
  /**
    * performs a synchronous run of a MATSim simulation from a MATSimConfig
    * @return either a matsim config setup error, or,
    *         the final state of the simulation after the simulation ends (which could be a failure as well)
    */
  def run(): Either[io.Serializable, Any] = {
    for {
      network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(config.io.matsimNetworkFile)
      agentsUnderControl <- PopulationOps.loadAgentsUnderControl(config.io.populationFile)
    } yield {

      // wrap config along with MATSim-specific environment values
      val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(agentsUnderControl, config)

      Files.createDirectories(config.io.experimentDirectory)
      val experiment = new LocalMATSimRoutingExperiment(
        new File(config.io.experimentLoggingDirectory.resolve("route.csv").toString),
        new File(config.io.experimentLoggingDirectory.resolve("final.log").toString)
      )

      val routingAlgorithm: RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR] = matsimRunConfig.algorithm match {
        case MATSimConfig.Algorithm.Selfish =>
          // need a no-phase dijkstra's algorithm here?
          ???
        case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>

          systemOptimal.selectionAlgorithm match {
            case local: LocalMCTSSelection =>
              // special effect handling for MCTS library
              new TwoPhaseLocalMCTSRoutingAlgorithm[Coordinate, EdgeBPR](
                altPathsAlgorithm = systemOptimal.kspAlgorithm.build(),
                selectionAlgorithm = local.build(),
                pathToMarginalFlowsFunction = systemOptimal.pathToMarginalFlowsFunction.build(),
                combineFlowsFunction = systemOptimal.combineFlowsFunction.build(),
                marginalCostFunction = systemOptimal.marginalCostFunction.build()
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



      val experimentSyncIO: SyncIO[experiment.ExperimentState] =
        matsimRunConfig.algorithm match {
          case MATSimConfig.Algorithm.Selfish =>
            // need a no-batching manager version here? or, a dummy for now?
            ???
          case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
            experiment.run(
              config = matsimRunConfig,
              roadNetwork = network,
              routingAlgorithm = routingAlgorithm,
              updateFunction = EdgeBPRUpdateOps.edgeUpdateWithFlowCountDelta, // <- comes from same source that will feed routingAlgorithm above
              batchingFunction = systemOptimal.batchingFunction.build(),
              batchWindow = matsimRunConfig.routing.batchWindow,
              doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime
            )

        }

      val result: experiment.ExperimentState = experimentSyncIO.unsafeRunSync()
      result.error.foreach(e => logger.error(e))
      "done"
    }
  }
}
