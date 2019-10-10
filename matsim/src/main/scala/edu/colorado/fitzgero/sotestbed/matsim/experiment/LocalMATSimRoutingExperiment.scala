package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import edu.colorado.fitzgero.sotestbed.matsim.MATSimSimulation
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

final class LocalMATSimRoutingExperiment(routingResultFile: File, finalReportFile: File)
    extends AbstractMATSimRoutingExperiment[LocalMATSimRoutingExperiment.EmptyVertex.type, EdgeBPR](routingResultFile, finalReportFile) {

  override type Simulator              = MATSimSimulation
  override type SimulatorConfiguration = MATSimConfig

}

object LocalMATSimRoutingExperiment {
  final object EmptyVertex
}
