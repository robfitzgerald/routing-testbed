package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import kantan.csv._
import kantan.csv.ops._

case class AgentPerformanceMetrics(
  ttDiffMin: Double = 0.0,
  ttDiffNorm: Double = 0.0,
  distDiffMiles: Double = 0.0,
  distDiffNorm: Double = 0.0,
  speedDiffMph: Double = 0.0,
  speedDiffNorm: Double = 0.0,
  count: Int = 0,
  metricsSampledRatio: Double = 0.0
) {

  override def toString: String = {
    val ttDiffNormString: String     = AgentPerformanceMetrics.ratioToPercent(ttDiffNorm)
    val distDiffNormString: String   = AgentPerformanceMetrics.ratioToPercent(distDiffNorm)
    val speedDiffNormString: String  = AgentPerformanceMetrics.ratioToPercent(speedDiffNorm)
    val metricsSampledString: String = AgentPerformanceMetrics.ratioToPercent(metricsSampledRatio)
    f"$ttDiffMin%.2f,$ttDiffNormString,$distDiffMiles%.2f,$distDiffNormString,$speedDiffMph%.2f,$speedDiffNormString,$metricsSampledString"
  }

}

object AgentPerformanceMetrics extends LazyLogging {

  val Header: String = "ttDiffMin,ttDiffNorm,distDiffMiles,distDiffNorm,speedDiffMph,speedDiffNorm,metricsSamplePct"

  def headerWithPrefix(prefix: String): String =
    Header
      .split(",")
      .map { colName => f"$prefix$colName" }
      .mkString(",")

  def ratioToPercent(n: Double): String = f"${n * 100.0}%.2f%%"

  val SpeedUpperBoundMph: Double = 80.0

  // /**
  //   * given a running MATSimRunConfig for an optimal experiment, computes the performance metrics
  //   *
  //   * @param config the matsim config for an optimal experiment
  //   * @return performance metrics
  //   */
  // def fromConfig(config: MATSimRunConfig): Either[Exception, AgentPerformanceMetrics] = {
  //   val selfishConfig: MATSimRunConfig =
  //     config.copy(scenarioData = config.scenarioData.copy(algorithm = "selfish"))
  //   val selfishAgentExperienceFile: File =
  //     selfishConfig.experimentLoggingDirectory.resolve("agentExperience.csv").toFile
  //   val optimalAgentExperienceFile: File =
  //     config.experimentLoggingDirectory.resolve("agentExperience.csv").toFile

  //   fromFiles(selfishAgentExperienceFile, optimalAgentExperienceFile)
  // }

  /**
    * given two agent experience files, compute the performance metrics measured by difference values
    *
    * @param referenceAgentExperienceFile
    * @param thisAgentExperienceFile
    * @return
    */
  def fromFiles(
    referenceAgentExperienceFile: File,
    thisAgentExperienceFile: File,
    reportImprovedAgents: Boolean = true,
    reportDisImprovedAgents: Boolean = true,
    soAgentsOnly: Boolean = false
  ): Either[Exception, AgentPerformanceMetrics] = {

    implicit val dec: HeaderDecoder[AgentExperienceRow] = AgentExperienceRow.headerDecoder

    Try {

      // pull in all data (these unsafe readers can throw)
      val selfishRows: Map[String, AgentExperienceRow] =
        referenceAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row => s"${row.agentId}#${row.departureTime}" -> row }
          .toMap
      val optimalRows: Map[String, AgentExperienceRow] =
        thisAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row => s"${row.agentId}#${row.departureTime}" -> row }
          .toMap

      case class Acc(
        ttS: Double = 0.0,
        ttO: Double = 0.0,
        distS: Double = 0.0,
        distO: Double = 0.0,
        speedS: Double = 0.0,
        speedO: Double = 0.0,
        count: Int = 0
      ) {
        def add(ttS: Double, ttO: Double, distS: Double, distO: Double, speedS: Double, speedO: Double): Acc = {
          this.copy(
            ttS = this.ttS + ttS,
            ttO = this.ttO + ttO,
            distS = this.distS + distS,
            distO = this.distO + distO,
            speedS = this.speedS + speedS,
            speedO = this.speedO + speedO,
            count = this.count + 1
          )
        }

        def avgTravelTimeDiff: Double      = if (count == 0.0) 0 else (ttO - ttS) / count
        def avgTravelTimeDiffRatio: Double = if (ttS == 0.0) 0 else (ttO - ttS) / ttS
        def avgDistanceDiff: Double        = if (count == 0.0) 0 else (distO - distS) / count
        def avgDistanceDiffRatio: Double   = if (distS == 0.0) 0 else (distO - distS) / distS
        def avgSpeedDiff: Double           = if (count == 0.0) 0 else (speedO - speedS) / count
        def avgSpeedDiffRatio: Double      = if (speedS == 0.0) 0 else (speedO - speedS) / speedS
      }

      // sum up each measure
      val diffsAccum: Acc =
        optimalRows.foldLeft(Acc()) {
          case (acc, (agentTimeIndex, optimalRow)) =>
            selfishRows.get(agentTimeIndex) match {
              case None =>
                acc
              case Some(selfishRow) =>
                if (optimalRow.travelTime == 0.0 || selfishRow.travelTime == 0.0) {
//                  logger.debug("encountered infinite mph speed, skipping")
                  acc
                } else if (soAgentsOnly && optimalRow.requestClass == "ue") {
                  acc
                } else {
                  val ttS: Double                    = selfishRow.travelTime / 60.0
                  val ttO: Double                    = optimalRow.travelTime / 60.0
                  val travelTimeWasImproved: Boolean = ttO < ttS
                  val distS: Double                  = selfishRow.distance / 1609.0
                  val distO: Double                  = optimalRow.distance / 1609.0
                  val speedO: Double                 = (optimalRow.distance / optimalRow.travelTime) * (3600.0 / 1609.0)
                  val speedS: Double                 = (selfishRow.distance / selfishRow.travelTime) * (3600.0 / 1609.0)

                  // report only results we are interested in
                  if (reportImprovedAgents && travelTimeWasImproved || reportDisImprovedAgents && !travelTimeWasImproved) {
                    val updated = acc.add(ttS, ttO, distS, distO, speedS, speedO)
                    updated
                  } else {
                    acc
                  }
                }
            }
        }

      // convert accumulated sums into averages
      val diffsAveraged: AgentPerformanceMetrics = AgentPerformanceMetrics(
        ttDiffMin = diffsAccum.avgTravelTimeDiff,
        ttDiffNorm = diffsAccum.avgTravelTimeDiffRatio,
        distDiffMiles = diffsAccum.avgDistanceDiff,
        distDiffNorm = diffsAccum.avgDistanceDiffRatio,
        speedDiffMph = diffsAccum.avgSpeedDiff,
        speedDiffNorm = diffsAccum.avgSpeedDiffRatio,
        count = diffsAccum.count,
        metricsSampledRatio = diffsAccum.count.toDouble / selfishRows.size.toDouble
      )

      diffsAveraged

    }.toEither.left.map { t => new Exception(t) }
  }
}
