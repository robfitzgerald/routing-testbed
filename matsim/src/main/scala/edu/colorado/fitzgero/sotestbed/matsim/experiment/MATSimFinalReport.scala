package edu.colorado.fitzgero.sotestbed.matsim.experiment

import java.io.{File, PrintWriter}

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.matsim.MATSimSimulation
import edu.colorado.fitzgero.sotestbed.reports.{FinalReports, RoutingResultFileReport}

class MATSimFinalReport(finalReportFile: File) extends FinalReports[IO] {
  override type Simulator = MATSimSimulation

  val printWriter: PrintWriter = new PrintWriter(finalReportFile)
  printWriter.write(RoutingResultFileReport.Header + "\n")

  def finishReports(simulator: MATSimSimulation): IO[Unit] = IO {
    printWriter.write("nothing reported yet\n")
    printWriter.close()
  }
}
