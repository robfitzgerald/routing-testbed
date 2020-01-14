package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.File

import cats.effect.SyncIO

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, TwoPhaseLocalMCTSRoutingAlgorithm, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.config.algorithm.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection}
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object MATSimExperimentApp extends App {

  val result: Either[io.Serializable, Any] = for {
    fileConfig <- ConfigSource.file("matsim/src/main/resources/matsim-conf/louisville/default-experiment.conf").load[MATSimConfig]
    network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(fileConfig.io.matsimNetworkFile)
    agentsUnderControl <- PopulationOps.loadAgentsUnderControl(fileConfig.io.populationFile)
  } yield {

//    val confEdit = config.copy(
//      io = config.io.copy(name = s"${config.io.name}-maxpath=$maxPathAssignments"),
//      routing = config.routing.copy(maxPathAssignments = maxPathAssignments)
//    )

    val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(agentsUnderControl, fileConfig)
    val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))

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

    experimentSyncIO.unsafeRunSync()
  }

  result match {
    case Left(error) =>
      println("run failure")
      println(error)
    //        System.exit(1)
    case Right(_) =>
      println("run success")

    //        System.exit(0)
  }
}
