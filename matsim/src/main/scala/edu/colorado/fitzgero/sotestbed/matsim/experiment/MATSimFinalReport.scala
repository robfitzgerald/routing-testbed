package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.{File, PrintWriter}

import cats.effect.{IO, SyncIO}

import edu.colorado.fitzgero.sotestbed.reports.{CompletePathAlternativesRoutingReport, FinalReports}

class MATSimFinalReport(finalReportFile: File) extends FinalReports[SyncIO] {

  val printWriter: PrintWriter = new PrintWriter(finalReportFile)
  printWriter.write(CompletePathAlternativesRoutingReport.Header + "\n")

  def finishReports(simulator: Any): SyncIO[Unit] = SyncIO {
    printWriter.write("nothing reported yet\n")
    printWriter.close()
  }
}
