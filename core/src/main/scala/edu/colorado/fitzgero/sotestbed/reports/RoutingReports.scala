package edu.colorado.fitzgero.sotestbed.reports

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

abstract class RoutingReports[F[_], V, E] {
  def updateReports(routingResult: List[(String, RoutingAlgorithm.Result)], roadNetwork: RoadNetwork[F, V, E], currentTime: SimTime): F[Unit]
  def close(): Unit
}
