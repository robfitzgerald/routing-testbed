package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.{File, PrintWriter}

import cats.effect.{IO, SyncIO}

import edu.colorado.fitzgero.sotestbed.reports.{FinalReports, RoutingResultFileReport}

class MATSimFinalReport(finalReportFile: File) extends FinalReports[SyncIO] {
  override type Simulator = Unit

  val printWriter: PrintWriter = new PrintWriter(finalReportFile)
  printWriter.write(RoutingResultFileReport.Header + "\n")

  def finishReports(simulator: Simulator): SyncIO[Unit] = SyncIO {
    printWriter.write("nothing reported yet\n")
    printWriter.close()
  }
}
