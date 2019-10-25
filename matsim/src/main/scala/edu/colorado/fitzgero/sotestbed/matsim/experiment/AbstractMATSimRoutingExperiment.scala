package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.File

import cats.effect.{IO, SyncIO}

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.matsim.{MATSimProxy, MATSimSimulation}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.reports.RoutingResultFileReport

abstract class AbstractMATSimRoutingExperiment[V, E] (
  routingResultFile: File,
  finalReportFile: File
) extends RoutingExperiment[SyncIO, V, E] with MATSimProxy {

  val routingResultFileReport: RoutingResultFileReport = new RoutingResultFileReport(routingResultFile)
  val finalReport: MATSimFinalReport = new MATSimFinalReport(finalReportFile)

  def updateReports(routingResult: RoutingAlgorithm.Result, currentSimTime: SimTime): Unit = routingResultFileReport.updateReports(routingResult, currentSimTime)

  def close(): Unit = routingResultFileReport.close()

  def finishReports(simulator: MATSimSimulation): SyncIO[Unit] = finalReport.finishReports(simulator)
}
