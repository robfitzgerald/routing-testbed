package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File
import scala.concurrent.ExecutionContext.global

import cats.effect.{ContextShift, IO}

import pureconfig._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.kSPwLO_SVP
import edu.colorado.fitzgero.sotestbed.algorithm.routing.TwoPhaseRoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.RandomSamplingSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.matsim.MATSimSimulation
import edu.colorado.fitzgero.sotestbed.matsim.experiment.{AbstractMATSimRoutingExperiment, LocalMATSimRoutingExperiment}
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object MATSimExperimentApp extends App {

  // TODO: consider using cats.effect.IOApp which provides a ContextShift[IO] by default
  // and possibly set the size of the thread pool via config instead of just using global here
  implicit val ctx: ContextShift[IO] = IO.contextShift(global)

  for {
    config  <- ConfigSource.file("matsim/src/main/resources/matsim-conf/default.conf").load[MATSimConfig]
    network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(config.fs.matsimNetworkFile)
  } yield {

    // TODO: population import, parse UE/SO for config
    val pop: MATSimRunConfig.Population = MATSimRunConfig.Population(
      ???
    )

    val matsimRunConfig: MATSimRunConfig = MATSimRunConfig(
      pop,
      config.fs,
      config.routing,
      config.run
    )

    val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))

    // TODO: case classes for MATSimConfig.Algorithm entries
    // see https://pureconfig.github.io/docs/overriding-behavior-for-sealed-families.html

    // TODO: mapping functions which take the sealed traits and construct the functions passed in below
    val routingAlgorithm = new TwoPhaseRoutingAlgorithm[IO, Coordinate, EdgeBPR](
      new kSPwLO_SVP[IO, Coordinate, EdgeBPR](theta = config.routing.theta),
      new RandomSamplingSelectionAlgorithm(0L),
      ???,
      ???,
      ???,
      ???,
      ???
    )

//    experiment.run(
//      matsimRunConfig,
//      network,
//      routingAlgorithm,
//      , // <- comes from same source that will feed routingAlgorithm above
//      config.run.endOfRoutingTime
//    )
  }

}
