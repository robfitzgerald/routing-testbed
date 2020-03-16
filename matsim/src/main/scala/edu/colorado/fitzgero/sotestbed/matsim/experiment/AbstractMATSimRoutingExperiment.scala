package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.{IO, SyncIO}

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.matsim.simulator.MATSimSimulator
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingResultFileReport

abstract class AbstractMATSimRoutingExperiment(
  routingResultFile: File,
  finalReportFile: File,
  costFunction: EdgeBPR => Cost
) extends RoutingExperiment[SyncIO, Coordinate, EdgeBPR]
    with MATSimSimulator {

  val routingResultFileReport: RoutingResultFileReport = new RoutingResultFileReport(routingResultFile, costFunction)
  val finalReport: MATSimFinalReport                   = new MATSimFinalReport(finalReportFile)

  override def updateReports(routingResult: List[RoutingAlgorithm.Result],
                             roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                             currentSimTime: SimTime): SyncIO[Unit] =
    routingResultFileReport.updateReports(routingResult, roadNetwork, currentSimTime)

  def close(): Unit = routingResultFileReport.close()

  override def finishReports(simulator: Any): SyncIO[Unit] = SyncIO { () } // kinda deprecated for now...
}
