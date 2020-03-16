package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

final class LocalMATSimRoutingExperiment(routingResultFile: File, finalReportFile: File, costFunction: EdgeBPR => Cost)
    extends AbstractMATSimRoutingExperiment(routingResultFile, finalReportFile, costFunction) {

  override type Simulator              = Unit
  override type SimulatorConfiguration = MATSimRunConfig

}
