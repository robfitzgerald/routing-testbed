package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

class RoutingResultFileReport(routingResultFile: File) extends RoutingReports {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(RoutingResultFileReport.Header + "\n")

  override def updateReports(routingResult: RoutingAlgorithm.Result, currentSimTime: SimTime): Unit = {

    val output = List(
      currentSimTime.value.toString,
      currentSimTime.toString,
      routingResult.kspRuntime.value.toString,
      routingResult.selectionRuntime.value.toString,
      routingResult.alternatives.keys.size.toString,
      routingResult.responses.length
    )

    printWriter.write(output.mkString("", ",", "\n"))
  }

  def close(): Unit = {
    printWriter.close()
  }
}


object RoutingResultFileReport {
  val Header: String = "simTimeValue,simTime,kspRuntime,selectRuntime,numRequests,numResponses"
}