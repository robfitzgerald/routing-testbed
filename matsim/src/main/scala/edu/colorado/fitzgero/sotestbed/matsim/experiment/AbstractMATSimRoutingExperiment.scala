package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.{IO, SyncIO}

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.matsim.simulator.MATSimSimulator
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.reports.RoutingResultFileReport

abstract class AbstractMATSimRoutingExperiment[V, E] (
  routingResultFile: File,
  finalReportFile: File
) extends RoutingExperiment[SyncIO, V, E] with MATSimSimulator {

  val routingResultFileReport: RoutingResultFileReport = new RoutingResultFileReport(routingResultFile)
  val finalReport: MATSimFinalReport = new MATSimFinalReport(finalReportFile)

  override def updateReports(routingResult: List[RoutingAlgorithm.Result], currentSimTime: SimTime): Unit = routingResultFileReport.updateReports(routingResult, currentSimTime)

  def close(): Unit = routingResultFileReport.close()

  override def finishReports(simulator: Simulator): SyncIO[Unit] = finalReport.finishReports(simulator)
}
