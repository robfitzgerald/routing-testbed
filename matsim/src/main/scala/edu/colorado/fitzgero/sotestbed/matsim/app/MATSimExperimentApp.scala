package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File

import cats.effect.SyncIO

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingOps, TwoPhaseLocalMCTSRoutingAlgorithm, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.RandomSamplingSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.config.algorithm.SelectionAlgorithmConfig.{LocalMCTSSelection, RandomSamplingSelection}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object MATSimExperimentApp extends App {

  val result = for {
    fileConfig <- ConfigSource.file("matsim/src/main/resources/matsim-conf/rye/default-experiment.conf").load[MATSimConfig]
    network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(fileConfig.io.matsimNetworkFile)
    agentsUnderControl <- PopulationOps.loadAgentsUnderControl(fileConfig.io.populationFile)
  } yield {

//    val confEdit = config.copy(
//      io = config.io.copy(name = s"${config.io.name}-maxpath=$maxPathAssignments"),
//      routing = config.routing.copy(maxPathAssignments = maxPathAssignments)
//    )

    val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(agentsUnderControl, fileConfig)
    val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))

    val routingAlgorithm = matsimRunConfig.algorithm.selectionAlgorithm match {
      case local: LocalMCTSSelection =>
        // special effect handling for MCTS library
        new TwoPhaseLocalMCTSRoutingAlgorithm[Coordinate, EdgeBPR](
          altPathsAlgorithm = matsimRunConfig.algorithm.kspAlgorithm.build(),
          selectionAlgorithm = local.build(),
          pathToMarginalFlowsFunction = matsimRunConfig.algorithm.pathToMarginalFlowsFunction.build(),
          combineFlowsFunction = matsimRunConfig.algorithm.combineFlowsFunction.build(),
          marginalCostFunction = matsimRunConfig.algorithm.marginalCostFunction.build()
        )
      case rand: RandomSamplingSelection =>
        // other libraries play well
        new TwoPhaseRoutingAlgorithm[SyncIO, Coordinate, EdgeBPR](
          altPathsAlgorithm = matsimRunConfig.algorithm.kspAlgorithm.build(),
          selectionAlgorithm = rand.build(),
          pathToMarginalFlowsFunction = matsimRunConfig.algorithm.pathToMarginalFlowsFunction.build(),
          combineFlowsFunction = matsimRunConfig.algorithm.combineFlowsFunction.build(),
          marginalCostFunction = matsimRunConfig.algorithm.marginalCostFunction.build()
        )
    }

    experiment.run(
      config = matsimRunConfig,
      roadNetwork = network,
      routingAlgorithm = routingAlgorithm,
      updateFunction = EdgeBPRUpdateOps.edgeUpdateWithFlowCountDelta, // <- comes from same source that will feed routingAlgorithm above
      batchingFunction = matsimRunConfig.algorithm.batchingFunction.build(),
      batchWindow = matsimRunConfig.routing.batchWindow,
      doneRoutingAtSimTime = matsimRunConfig.run.endOfRoutingTime
    )
  }

  result match {
    case Left(error) =>
      println("configuration failed")
      println(error)
    //        System.exit(1)
    case Right(experiment) =>
      println("running experiment")

      val result = experiment.unsafeRunSync()
      result.simulator


    //        System.exit(0)
  }
}
