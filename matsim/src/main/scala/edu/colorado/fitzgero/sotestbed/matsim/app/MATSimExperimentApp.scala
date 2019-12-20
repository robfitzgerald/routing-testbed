package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File


import cats.effect.SyncIO

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.{AltPathsAlgorithm, kSPwLO_SVP_Sync}
import edu.colorado.fitzgero.sotestbed.algorithm.batching.GreedyBatching
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingOps, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{RandomSamplingSelectionAlgorithm, SelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRCostOps, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object MATSimExperimentApp extends App {

  val result = for {
    config <- ConfigSource.file("matsim/src/main/resources/matsim-conf/louisville/default-experiment.conf").load[MATSimConfig]
    network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(config.io.matsimNetworkFile)
    agentsUnderControl <- PopulationOps.loadAgentsUnderControl(config.io.populationFile)
  } yield {

//    val confEdit = config.copy(
//      io = config.io.copy(name = s"${config.io.name}-maxpath=$maxPathAssignments"),
//      routing = config.routing.copy(maxPathAssignments = maxPathAssignments)
//    )

    val pop: MATSimRunConfig.Population = MATSimRunConfig.Population(agentsUnderControl)

    val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(
      pop,
      config.io,
      config.routing,
      config.run
    )

    val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))

    // TODO: the requested functions below should be parsed from config (see population config)
    //  see https://pureconfig.github.io/docs/overriding-behavior-for-sealed-families.html
    val routingAlgorithm = new TwoPhaseRoutingAlgorithm[SyncIO, Coordinate, EdgeBPR](
      altPathsAlgorithm = new kSPwLO_SVP_Sync[SyncIO, Coordinate, EdgeBPR](theta = config.routing.theta),
      selectionAlgorithm = new RandomSamplingSelectionAlgorithm(0L),
      pathToMarginalFlowsFunction = RoutingOps.defaultMarginalFlow,
      combineFlowsFunction = RoutingOps.defaultCombineFlows,
      marginalCostFunction = EdgeBPRCostOps.marginalCostFunction(0.15, 4.0),
      kspTerminationFunction = (state: AltPathsAlgorithm.AltPathsState) => state.alts.length == config.routing.k.value,
      selectionTerminationFunction = (state: SelectionAlgorithm.SelectionState) => state.startTime + 5000 < System.currentTimeMillis
    )

    experiment.run(
      config = matsimRunConfig,
      roadNetwork = network,
      routingAlgorithm = routingAlgorithm,
      updateFunction = EdgeBPRUpdateOps.edgeUpdateWithFlowCount, // <- comes from same source that will feed routingAlgorithm above
      batchingFunction = GreedyBatching(config.routing.batchWindow, config.routing.minimumReplanningWaitTime),
      batchWindow = config.routing.batchWindow,
      doneRoutingAtSimTime = config.run.endOfRoutingTime
    )
  }

  result match {
    case Left(error) =>
      println("configuration failed")
      println(error)
    //        System.exit(1)
    case Right(result) =>
      println("running experiment")

      result.unsafeRunSync()

    //        System.exit(0)
  }
}
