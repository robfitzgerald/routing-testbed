package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  * reports data at the batch level, ignoring individual agent results
  *
  * @param routingResultFile
  */
class BatchReporter(routingResultFile: File) extends RoutingReports[SyncIO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(BatchReporter.Header + "\n")

  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                    roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                    currentTime: SimTime): SyncIO[Unit] = SyncIO {

    for {
      (batchId, batchResult) <- routingResult
      if batchResult.responses.lengthCompare(1) > 0 && batchId != "ue" // only report SO batch assignments
    } {
      val batchSize: Int                 = batchResult.responses.size
      val totalAlts: Int                 = batchResult.kspResult.values.map { _.size }.sum
      val avgAlts: Double                = if (batchResult.kspResult.nonEmpty) totalAlts.toDouble / batchResult.kspResult.size else 0.0
      val assignedOptimalRoute: Double   = batchResult.responses.count { _.pathIndex != 0 }.toDouble
      val proportionOptimalRoute: Double = assignedOptimalRoute / batchSize
      val meanTravelTimeDiff: Double     = batchResult.meanTravelTimeDiff.value
      val samples: Int                   = batchResult.samples
      val searchSpaceSize: BigDecimal = if (batchResult.kspResult.nonEmpty) batchResult.kspResult.values.map { alts =>
        BigDecimal(alts.length)
      }.product
      else BigDecimal(0)

      val row: BatchReporter.Row = BatchReporter.Row(
        time = currentTime,
        batchId = batchId,
        size = batchSize,
        avgAlts = avgAlts,
        samples = samples,
        searchSpaceSize = searchSpaceSize,
        optimalProportion = proportionOptimalRoute,
        meanTravelTimeDiff = meanTravelTimeDiff,
      )

      printWriter.write(f"${row.toString}\n")
    }
  }

  def close(): Unit = {
    printWriter.close()
  }
}

object BatchReporter {

  val Header: String = "time,batch_id,batch_size,avgAltPathsPerAgent,searchSpaceSamples,searchSpaceSize,searchSpaceExplored,soAssignmentPercent"
  final case class Row(
    time: SimTime,
    batchId: String,
    size: Int,
    avgAlts: Double,
    samples: Int,
    searchSpaceSize: BigDecimal,
    optimalProportion: Double = 0.0,
    meanTravelTimeDiff: Double = 0.0,
  ) {

    override def toString: String = {
      val spaceExploredRatio: Double  = if (searchSpaceSize != BigDecimal(0)) (BigDecimal(samples) / searchSpaceSize).toDouble * 100.0 else 0.0
      val spaceExplored: String       = f"$spaceExploredRatio%.9f%%"
      val soAssignmentPercent: String = f"${optimalProportion * 100.0}%.2f%%"

      f"$time,$batchId,$size,$avgAlts%.2f,$samples,$searchSpaceSize,$spaceExplored,$soAssignmentPercent"
    }
  }
}
