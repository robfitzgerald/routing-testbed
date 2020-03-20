package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

final class LocalMATSimRoutingExperiment(finalReportFile: File, routingReports: RoutingReports[SyncIO, Coordinate, EdgeBPR])
    extends AbstractMATSimRoutingExperiment(finalReportFile, routingReports) {

  override type Simulator              = Unit
  override type SimulatorConfiguration = MATSimRunConfig

}
