package edu.colorado.fitzgero.sotestbed.reports

import java.io.{File, PrintWriter}

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class BatchLearningReporter(routingResultFile: File) extends RoutingReports[SyncIO, Coordinate, EdgeBPR] {

  val printWriter: PrintWriter = new PrintWriter(routingResultFile)
  printWriter.write(BatchLearningReporter.Header + "\n")

  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                    roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                    currentTime: SimTime): SyncIO[Unit] = SyncIO {

    for {
      (batchId, batchResult) <- routingResult
    } {

      val row: BatchLearningReporter.Row = if (batchResult.responses.lengthCompare(2) < 0) {
        // empty or singleton batch
        BatchLearningReporter.Row(
          time = currentTime,
          batchId = batchId,
          size = batchResult.responses.size,
          optimalProportion = 0.0,
          avgImprovement = 0.0
        )
      } else {
        // calculate SO batch analytics
        val batchSize: Int                 = batchResult.responses.size
        val assignedOptimalRoute: Double   = batchResult.responses.count { _.pathIndex != 0 }.toDouble
        val proportionOptimalRoute: Double = assignedOptimalRoute / batchSize
        val avgImprovement: Double         = batchResult.avgTravelTimeImprovement.value

        BatchLearningReporter.Row(
          time = currentTime,
          batchId = batchId,
          size = batchSize,
          optimalProportion = proportionOptimalRoute,
          avgImprovement = avgImprovement
        )
      }

      printWriter.write(f"${row.toString}\n")
    }
  }

  def close(): Unit = printWriter.close()
}

object BatchLearningReporter {

  val Header: String = "time,batch_id,size,optimal_proportion,avg_improvement"
  final case class Row(
    time: SimTime,
    batchId: String,
    size: Int,
    optimalProportion: Double,
    avgImprovement: Double,
  ) {
    override def toString: String = f"$time,$batchId,$size,$optimalProportion%.2f,$avgImprovement%.2f"
  }
}
