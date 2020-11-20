package edu.colorado.fitzgero.sotestbed.reports

import java.nio.file.Path

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class AvgSpeedHeatmapReport(avgSpeedHeatmap: AvgSpeedHeatmap, logCycle: SimTime) extends RoutingReports[IO, Coordinate, EdgeBPR] with LazyLogging {

  /**
    * adds rows to a file with heatmap outputs
    *
    * @param routingResult
    * @param roadNetwork
    * @param currentTime
    * @return
    */
  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
                    currentTime: SimTime): IO[Unit] = IO {

    if (currentTime % logCycle == SimTime.Zero) {
      logger.debug(f"writing avg speed heatmap log at time $currentTime")
      avgSpeedHeatmap.appendHeatmapWithTimeWindowedData(roadNetwork, currentTime)
    } else {
      logger.debug(f"no avg speed heatmap log at time $currentTime")
    }
  }

  def close(): Unit = avgSpeedHeatmap.close()
}

object AvgSpeedHeatmapReport {

  def apply(loggingDirectory: Path,
            logCycle: SimTime,
            h3Resolution: Int,
            initialGraph: RoadNetwork[IO, Coordinate, EdgeBPR],
            costFunction: EdgeBPR => Cost): AvgSpeedHeatmapReport = {

    val avgSpeedHeatmap = AvgSpeedHeatmap(
      initialGraph = initialGraph,
      loggingDirectory = loggingDirectory,
      costFunction = costFunction,
      h3Resolution = h3Resolution
    )

    new AvgSpeedHeatmapReport(avgSpeedHeatmap, logCycle)
  }
}
