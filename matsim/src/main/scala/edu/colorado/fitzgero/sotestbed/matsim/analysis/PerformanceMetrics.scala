package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File

import scala.util.Try

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import kantan.csv._
import kantan.csv.ops._

case class PerformanceMetrics(
  ttDiffMin: Double = 0.0,
  ttDiffNorm: Double = 0.0,
  distDiffMiles: Double = 0.0,
  distDiffNorm: Double = 0.0,
  speedDiffMph: Double = 0.0,
  speedDiffNorm: Double = 0.0,
  count: Int = 0,
  metricsSampledRatio: Double = 0.0
) {

  def avg: PerformanceMetrics = PerformanceMetrics(
    ttDiffMin = this.ttDiffMin / count,
    ttDiffNorm = this.ttDiffNorm / count,
    distDiffMiles = this.distDiffMiles / count,
    distDiffNorm = this.distDiffNorm / count,
    speedDiffMph = this.speedDiffMph / count,
    speedDiffNorm = this.speedDiffNorm / count,
    count
  )

  def addMetricsSampledPct(metricsSampledRatio: Double): PerformanceMetrics =
    this.copy(metricsSampledRatio = metricsSampledRatio)

  override def toString: String = {
    val ttDiffNormString: String     = PerformanceMetrics.ratioToPercent(ttDiffNorm)
    val distDiffNormString: String   = PerformanceMetrics.ratioToPercent(distDiffNorm)
    val speedDiffNormString: String  = PerformanceMetrics.ratioToPercent(speedDiffNorm)
    val metricsSampledString: String = PerformanceMetrics.ratioToPercent(metricsSampledRatio)
    f"$ttDiffMin%.2f,$ttDiffNormString,$distDiffMiles%.2f,$distDiffNormString,$speedDiffMph%.2f,$speedDiffNormString,$metricsSampledString"
  }

}

object PerformanceMetrics {
  val Header: String = "ttDiffMin,ttDiffNorm,distDiffMiles,distDiffNorm,speedDiffMph,speedDiffNorm,metricsSamplePct"

  def ratioToPercent(n: Double): String = f"${n * 100.0}%.2f%%"

  val SpeedUpperBoundMph: Double = 80.0

  /**
    * given a running MATSimRunConfig for an optimal experiment, computes the performance metrics
    *
    * @param config the matsim config for an optimal experiment
    * @return performance metrics
    */
  def computePerformanceMetrics(config: MATSimRunConfig): Either[Exception, PerformanceMetrics] = {
    val selfishConfig: MATSimRunConfig =
      config.copy(scenarioData = config.scenarioData.copy(algorithm = "selfish"))
    val selfishAgentExperienceFile: File =
      selfishConfig.experimentLoggingDirectory.resolve("agentExperience.csv").toFile
    val optimalAgentExperienceFile: File =
      config.experimentLoggingDirectory.resolve("agentExperience.csv").toFile

    computePerformanceMetrics(selfishAgentExperienceFile, optimalAgentExperienceFile)
  }

  /**
    * given two agent experience files, compute the performance metrics measured by difference values
    *
    * @param referenceAgentExperienceFile
    * @param thisAgentExperienceFile
    * @return
    */
  def computePerformanceMetrics(referenceAgentExperienceFile: File, thisAgentExperienceFile: File): Either[Exception, PerformanceMetrics] = {

    implicit val dec: HeaderDecoder[AgentExperienceRow] = AgentExperienceRow.headerDecoder

    Try {

      // pull in all data (these unsafe readers can throw)
      val selfishRows: Map[String, AgentExperienceRow] =
        referenceAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row =>
            s"${row.agentId}#${row.departureTime}" -> row
          }
          .toMap
      val optimalRows: Map[String, AgentExperienceRow] =
        thisAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row =>
            s"${row.agentId}#${row.departureTime}" -> row
          }
          .toMap

      // sum up each measure
      val diffsAccum: PerformanceMetrics =
        optimalRows.foldLeft(PerformanceMetrics()) {
          case (acc, (agentTimeIndex, optimalRow)) =>
            selfishRows.get(agentTimeIndex) match {
              case None =>
                acc
              case Some(selfishRow) =>
                val speedO = (optimalRow.distance / optimalRow.travelTime) * (3600.0 / 1609.0)
                val speedS = (selfishRow.distance / selfishRow.travelTime) * (3600.0 / 1609.0)
                acc.copy(
                  ttDiffMin = acc.ttDiffMin + (optimalRow.travelTime - selfishRow.travelTime) / 60.0,
                  ttDiffNorm = acc.ttDiffNorm + (optimalRow.travelTime - selfishRow.travelTime) / selfishRow.travelTime,
                  distDiffMiles = acc.distDiffMiles + (optimalRow.distance - selfishRow.distance) / 1609.0,
                  distDiffNorm = acc.distDiffNorm + (optimalRow.distance - selfishRow.distance) / selfishRow.distance,
                  speedDiffMph = acc.speedDiffMph + math.min(speedO - speedS, SpeedUpperBoundMph),
                  speedDiffNorm = acc.speedDiffNorm + (speedO - speedS) / speedS,
                  count = acc.count + 1
                )
            }
        }

      // results
      val diffsAveraged: PerformanceMetrics = diffsAccum.avg.addMetricsSampledPct(diffsAccum.count.toDouble / selfishRows.size.toDouble)

      diffsAveraged

    }.toEither.left.map { t =>
      new Exception(t)
    }
  }
}
