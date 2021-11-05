package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.{File, PrintWriter}

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.reports.{CompletePathAlternativesRoutingReport, FinalReports}

class MATSimFinalReport(finalReportFile: File) extends FinalReports[IO] {

  val printWriter: PrintWriter = new PrintWriter(finalReportFile)
  printWriter.write(CompletePathAlternativesRoutingReport.Header + "\n")

  def finishReports(): IO[Unit] = IO {
    printWriter.write("nothing reported yet\n")
    printWriter.close()
  }
}
