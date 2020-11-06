package edu.colorado.fitzgero.sotestbed.matsim.analysis

import edu.colorado.fitzgero.sotestbed.matsim.analysis.AgentExperienceOps.{meanStdev, performanceMetricsAgg, performanceMetricsPGFPlotsAgg}

case class PopulationAggregateMetrics(
  outputString: String,
  allData: PGFPlotsData,
  winPercent: Double,
  losePercent: Double,
  loserData: PGFPlotsData
)

object PopulationAggregateMetrics {

  def from(experimentName: String, group: List[AgentMetrics]): PopulationAggregateMetrics = {
    val trials: Int = group.length

    // overall metrics
    val (ttMin, ttStdev)       = meanStdev(group.map { _.overallMetrics.travelTimeMinutes })
    val (distMiles, distStdev) = meanStdev(group.map { _.overallMetrics.distanceMiles })
    val (speedMph, speedStdev) = meanStdev(group.map { _.overallMetrics.speedMph })
    val overallMetrics         = f"$ttMin%.4f,$ttStdev%.4f,$distMiles%.4f,$distStdev%.4f,$speedMph%.4f,$speedStdev%.4f"

    // all performance metrics
    val allPerf: String  = performanceMetricsAgg(group.map { _.allPerformanceMetrics })
    val winPerf: String  = performanceMetricsAgg(group.map { _.winnerMetrics })
    val losePerf: String = performanceMetricsAgg(group.map { _.loserMetrics })

    // percent of agents winning/losing
    val overallCount: Int   = group.map { _.allPerformanceMetrics.count }.foldLeft(0) { _ + _ }
    val winCount: Int       = group.map { _.winnerMetrics.count }.foldLeft(0) { _ + _ }
    val loseCount: Int      = group.map { _.loserMetrics.count }.foldLeft(0) { _ + _ }
    val winPercent: Double  = (winCount.toDouble / overallCount.toDouble) * 100.0
    val losePercent: Double = (loseCount.toDouble / overallCount.toDouble) * 100.0
    val (ttNormAll, ttNormStdevAll, distNormAll, distNormStdevAll, speedNormAll, speedNormStdevAll) = performanceMetricsPGFPlotsAgg(group.map {
      _.allPerformanceMetrics
    })
    val (ttNormL, ttNormStdevL, distNormL, distNormStdevL, speedNormL, speedNormStdevL) = performanceMetricsPGFPlotsAgg(group.map { _.loserMetrics })

    val outputString = f"$experimentName,$trials,$overallMetrics,$winPercent%%,$losePercent%%,$allPerf,$winPerf,$losePerf"
    val allData = PGFPlotsData(
      experimentName,
      winPercent,
      losePercent,
      ttNormAll,
      ttNormStdevAll,
      distNormAll,
      distNormStdevAll,
      speedNormAll,
      speedNormStdevAll
    )

    val loserData = PGFPlotsData(
      experimentName,
      winPercent,
      losePercent,
      ttNormL,
      ttNormStdevL,
      distNormL,
      distNormStdevL,
      speedNormL,
      speedNormStdevL
    )

    PopulationAggregateMetrics(outputString, allData, winPercent, losePercent, loserData)
  }
}
