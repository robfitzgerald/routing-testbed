package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

class RoutingResultFileReport(routingResultFile: File) extends RoutingReports {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(RoutingResultFileReport.Header + "\n")

  override def updateReports(routingResults: List[RoutingAlgorithm.Result], currentSimTime: SimTime): Unit = {

    for {
      routingResult <- routingResults
    } {
      val avgAltsPerAgent: Double =
        if (routingResult.alternatives.isEmpty) 0
        else routingResult.alternatives.map{_._2.length}.sum.toDouble / routingResult.alternatives.size
      val output = List(
        currentSimTime.value.toString,
        currentSimTime.toString,
        f"${routingResult.kspRuntime.value}%.2f",
        f"${routingResult.selectionRuntime.value}%.2f",
        routingResult.alternatives.keys.size.toString,
        routingResult.responses.length,
        f"$avgAltsPerAgent%.2f"
      )

      printWriter.write(output.mkString("", ",", "\n"))
    }
  }

  def close(): Unit = {
    printWriter.close()
  }
}


object RoutingResultFileReport {
  val Header: String = "simTimeValue,simTime,kspRuntime,selectRuntime,numRequests,numResponses,avgAlts"
}