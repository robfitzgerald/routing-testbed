package edu.colorado.fitzgero.sotestbed.reports

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class NoRoutingReporter extends RoutingReports[IO, Coordinate, EdgeBPR] {

  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
                    currentTime: SimTime): IO[Unit] =
    IO { () }

  def close(): Unit = ()
}
