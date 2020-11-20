package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment2
import edu.colorado.fitzgero.sotestbed.matsim.simulator.MATSimSimulator
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.RoutingReports

abstract class AbstractMATSimRoutingExperiment2(
  finalReportFile: File,
  routingReports: RoutingReports[IO, Coordinate, EdgeBPR]
) extends RoutingExperiment2[Coordinate]
    with MATSimSimulator {

  val routingResultFileReport: RoutingReports[IO, Coordinate, EdgeBPR] = routingReports
  val finalReport: MATSimFinalReport                                   = new MATSimFinalReport(finalReportFile)

  override def updateReports(
    routingResult: List[(String, RoutingAlgorithm.Result)],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    currentSimTime: SimTime
  ): IO[Unit] =
    routingResultFileReport.updateReports(routingResult, roadNetwork, currentSimTime)

  def close(): Unit = routingResultFileReport.close()

  override def finishReports(): IO[Unit] = IO { () } // kinda deprecated for now...
}
