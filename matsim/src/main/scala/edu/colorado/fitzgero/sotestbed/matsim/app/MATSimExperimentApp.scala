package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io
import java.io.File

import cats.effect.SyncIO

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, RoutingOps, SelfishSyncRoutingBPR, TwoPhaseLocalMCTSRoutingAlgorithm, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.config.algorithm.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection}
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRCostOps, EdgeBPRUpdateOps}
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

    val soRoutingAlgorithm: RoutingAlgorithm[SyncIO, Coordinate, EdgeBPR] = matsimRunConfig.algorithm match {
      case MATSimConfig.Algorithm.Selfish =>
        // need a no-phase dijkstra's algorithm here?
        MATSimConfig.Algorithm.Selfish.build()
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
          experiment.run(
            config = matsimRunConfig,
            roadNetwork = network,
            ueRoutingAlgorithm = ueRoutingAlgorithm,
            soRoutingAlgorithm = soRoutingAlgorithm,
            updateFunction = EdgeBPRUpdateOps.edgeUpdateWithFlowCountDelta, // <- comes from same source that will feed routingAlgorithm above
            batchingFunction = MATSimConfig.Algorithm.Selfish.batchingStub,
            batchWindow = matsimRunConfig.routing.batchWindow,
            minBatchSize = matsimRunConfig.routing.minBatchSize,
            requestUpdateCycle = matsimRunConfig.routing.requestUpdateCycle,
            doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime,
            selfishOnly = matsimRunConfig.algorithm.selfishOnly
          )
        case systemOptimal: MATSimConfig.Algorithm.SystemOptimal =>
          experiment.run(
            config = matsimRunConfig,
            roadNetwork = network,
            ueRoutingAlgorithm = ueRoutingAlgorithm,
            soRoutingAlgorithm = soRoutingAlgorithm,
            updateFunction = EdgeBPRUpdateOps.edgeUpdateWithFlowCountDelta, // <- comes from same source that will feed routingAlgorithm above
            batchingFunction = systemOptimal.batchingFunction.build(),
            batchWindow = matsimRunConfig.routing.batchWindow,
            minBatchSize = matsimRunConfig.routing.minBatchSize,
            requestUpdateCycle = matsimRunConfig.routing.requestUpdateCycle,
            doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime,
            selfishOnly = matsimRunConfig.algorithm.selfishOnly
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
