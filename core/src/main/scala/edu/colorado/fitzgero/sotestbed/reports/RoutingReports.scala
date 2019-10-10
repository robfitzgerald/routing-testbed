package edu.colorado.fitzgero.sotestbed.reports

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

trait RoutingReports {
  def updateReports(routingResult: RoutingAlgorithm.Result, currentSimTime: SimTime): Unit
  def close(): Unit
}
