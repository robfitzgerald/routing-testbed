package edu.colorado.fitzgero.sotestbed.matsim.analysis

case class CompleteMetrics(
  nodeName: String,
  experimentName: String,
  overallMetrics: OverallMetrics,
  allPerformanceMetrics: PerformanceMetrics,
  winnerMetrics: PerformanceMetrics,
  loserMetrics: PerformanceMetrics
)
