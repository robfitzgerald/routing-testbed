package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import edu.colorado.fitzgero.sotestbed.matsim.MATSimSimulation
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

final class LocalMATSimRoutingExperiment(routingResultFile: File, finalReportFile: File)
    extends AbstractMATSimRoutingExperiment[Coordinate, EdgeBPR](routingResultFile, finalReportFile) {

  override type Simulator              = MATSimSimulation
  override type SimulatorConfiguration = MATSimRunConfig

}
