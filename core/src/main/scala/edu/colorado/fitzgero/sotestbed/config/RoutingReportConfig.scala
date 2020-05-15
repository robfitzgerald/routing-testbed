package edu.colorado.fitzgero.sotestbed.config

import java.io.File

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.{
  AggregateAgentDataRoutingReport,
  BatchLearningReporter,
  CompletePathAlternativesRoutingReport,
  NoRoutingReporter,
  RoutingReports
}

sealed trait RoutingReportConfig

object RoutingReportConfig {
  final case object CompletePath extends RoutingReportConfig {

    def build(routingResultFile: File, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] =
      new CompletePathAlternativesRoutingReport(routingResultFile, costFunction)
  }
  final case object AggregateData extends RoutingReportConfig {

    def build(routingResultFile: File, costFunction: EdgeBPR => Cost): RoutingReports[SyncIO, Coordinate, EdgeBPR] =
      new AggregateAgentDataRoutingReport(routingResultFile, costFunction)
  }

  final case object Inactive extends RoutingReportConfig {

    def build(): RoutingReports[SyncIO, Coordinate, EdgeBPR] = new NoRoutingReporter()
  }

  final case object BatchLearning extends RoutingReportConfig {

    def build(routingResultFile: File): RoutingReports[SyncIO, Coordinate, EdgeBPR] = new BatchLearningReporter(routingResultFile)
  }
}
