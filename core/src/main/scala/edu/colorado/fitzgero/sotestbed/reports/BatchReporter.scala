package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import cats.effect.IO

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
class BatchReporter(routingResultFile: File) extends RoutingReports[IO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(BatchReporter.Header + "\n")

  def updateReports(
    routingResult: List[(String, RoutingAlgorithm.Result)],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    currentTime: SimTime
  ): IO[Unit] = IO {

    for {
      (batchId, batchResult) <- routingResult
      if batchResult.responses.lengthCompare(1) > 0 && batchId != "ue" // only report SO batch assignments
    } {
      val batchSize: Int                 = batchResult.responses.size
      val totalAlts: Int                 = batchResult.kspResult.values.map { _.size }.sum
      val avgAlts: Double                = if (batchResult.kspResult.nonEmpty) totalAlts.toDouble / batchResult.kspResult.size else 0.0
      val assignedOptimalRoute: Double   = batchResult.responses.count { _.pathIndex != 0 }.toDouble
      val proportionOptimalRoute: Double = if (batchSize == 0) 0.0 else assignedOptimalRoute / batchSize
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
        selfishCost = batchResult.selfishCost.value,
        optimalCost = batchResult.optimalCost.value,
        travelTimeDiff = batchResult.travelTimeDiff.value,
        meanTravelTimeDiff = meanTravelTimeDiff,
        travelTimeDiffPercent = batchResult.percentTTDiff
      )

      printWriter.write(f"${row.toString}\n")
      printWriter.flush()
    }
  }

  def close(): Unit = {
    printWriter.close()
  }
}

object BatchReporter {

  val Header: String =
    "time,batch_id,batch_size,avgAltPathsPerAgent,searchSpaceSamples,searchSpaceSize,searchSpaceExplored,soAssignmentPercent,,ttSelfish,ttOptimal,ttDiff,ttMeanDiff,ttDiffPercent"

  final case class Row(
    time: SimTime,
    batchId: String,
    size: Int,
    avgAlts: Double,
    samples: Int,
    searchSpaceSize: BigDecimal,
    optimalProportion: Double,
    selfishCost: Double,
    optimalCost: Double,
    travelTimeDiff: Double,
    meanTravelTimeDiff: Double,
    travelTimeDiffPercent: String
  ) {

    override def toString: String = {
      val spaceExploredRatio: Double =
        if (searchSpaceSize != BigDecimal(0)) (BigDecimal(samples) / searchSpaceSize).toDouble * 100.0 else 0.0
      val spaceExplored: String       = f"$spaceExploredRatio%.9f%%"
      val soAssignmentPercent: String = f"${optimalProportion * 100.0}%.2f%%"

      f"$time,$batchId,$size,$avgAlts%.2f,$samples,$searchSpaceSize,$spaceExplored,$soAssignmentPercent,,$selfishCost%.2f,$optimalCost%.2f,$travelTimeDiff%.2f,$meanTravelTimeDiff%.2f,$travelTimeDiffPercent"
    }
  }
}
