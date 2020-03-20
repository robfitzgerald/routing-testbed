package edu.colorado.fitzgero.sotestbed.config

import java.io.File

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.{AggregateDataRoutingReport, CompletePathAlternativesRoutingReport, RoutingReports}

sealed trait RoutingReportConfig

object RoutingReportConfig {
  final case object CompletePath extends RoutingReportConfig {

    def build(routingResultFile: File, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] =
      new CompletePathAlternativesRoutingReport(routingResultFile, costFunction)
  }
  final case object AggregateData extends RoutingReportConfig {

    def build(routingResultFile: File, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] =
      new AggregateDataRoutingReport(routingResultFile, costFunction)
  }
}
