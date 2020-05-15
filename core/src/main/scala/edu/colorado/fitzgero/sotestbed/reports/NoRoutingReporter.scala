package edu.colorado.fitzgero.sotestbed.reports

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

class NoRoutingReporter extends RoutingReports[SyncIO, Coordinate, EdgeBPR] {

  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)],
                    roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
                    currentTime: SimTime): SyncIO[Unit] =
    SyncIO { () }

  def close(): Unit = ()
}
