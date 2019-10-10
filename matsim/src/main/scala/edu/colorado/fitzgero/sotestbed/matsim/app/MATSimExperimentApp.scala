package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.File

import edu.colorado.fitzgero.sotestbed.matsim.experiment.{AbstractMATSimRoutingExperiment, LocalMATSimRoutingExperiment}
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig

object MATSimExperimentApp extends App {
  val experiment = new LocalMATSimRoutingExperiment(new File("route.csv"), new File("final.log"))
//  val MATSimConfig: MATSimConfig = MATSimConfig(
//
//  )
//  experiment.run(
//
//  )
}
