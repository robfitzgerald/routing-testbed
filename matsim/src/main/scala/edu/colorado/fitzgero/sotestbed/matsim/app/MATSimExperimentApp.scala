package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File

import scala.concurrent.ExecutionContext.global

import cats.effect.{ContextShift, IO, SyncIO}

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.{AltPathsAlgorithm, kSPwLO_SVP_Sync}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingOps, TwoPhaseRoutingAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{RandomSamplingSelectionAlgorithm, SelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.matsim.experiment.LocalMATSimRoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.PopulationOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRCostOps, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object MATSimExperimentApp extends App {

  // TODO: consider using cats.effect.IOApp which provides a ContextShift[IO] by default
  // and possibly set the size of the thread pool via config instead of just using global here
//  implicit val ctx: ContextShift[IO] = IO.contextShift(global)

  val result = for {
    config  <- ConfigSource.file("matsim/src/main/resources/matsim-conf/default-experiment.conf").load[MATSimConfig]
    network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(config.fs.matsimNetworkFile)
    agentsUnderControl <- PopulationOps.loadAgentsUnderControl(config.fs.populationFile)
  } yield {

    val pop: MATSimRunConfig.Population = MATSimRunConfig.Population(
      agentsUnderControl
    )

    val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(
      pop,
      config.fs,
      config.routing,
      config.run
    )

    val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))

    // TODO: the requested functions below should be parsed from config (see population config)
    //  see https://pureconfig.github.io/docs/overriding-behavior-for-sealed-families.html
    val routingAlgorithm = new TwoPhaseRoutingAlgorithm[SyncIO, Coordinate, EdgeBPR](
      new kSPwLO_SVP_Sync[SyncIO, Coordinate, EdgeBPR](theta = config.routing.theta),
      new RandomSamplingSelectionAlgorithm(0L),
      RoutingOps.defaultMarginalFlow,
      RoutingOps.defaultCombineFlows,
      EdgeBPRCostOps.marginalCostFunction(0.15, 4.0),
      (state: AltPathsAlgorithm.AltPathsState) => state.alts.length == config.routing.k.value,
      (state: SelectionAlgorithm.SelectionState) => state.startTime + 5000 < System.currentTimeMillis
    )

    experiment.run(
      matsimRunConfig,
      network,
      routingAlgorithm,
      EdgeBPRUpdateOps.edgeUpdateWithFlowCount, // <- comes from same source that will feed routingAlgorithm above
      config.run.endOfRoutingTime
    )
  }

  result match {
    case Left(error) =>
      println("configuration failed")
      println(error)
      System.exit(1)
    case Right(result) =>
      println("running experiment")

      result.unsafeRunSync()

      System.exit(0)
  }
}
