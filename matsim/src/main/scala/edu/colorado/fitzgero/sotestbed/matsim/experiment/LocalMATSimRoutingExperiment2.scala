package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

final class LocalMATSimRoutingExperiment2(
  finalReportFile: File,
  routingReports: RoutingReports[IO, Coordinate, EdgeBPR]
) extends AbstractMATSimRoutingExperiment2(finalReportFile, routingReports) {

  override type SimulatorConfiguration = MATSimRunConfig

}
