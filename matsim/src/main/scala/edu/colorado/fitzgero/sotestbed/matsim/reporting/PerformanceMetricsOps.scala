package edu.colorado.fitzgero.sotestbed.matsim.reporting

import java.io.File

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import kantan.csv._
import kantan.csv.ops._

object PerformanceMetricsOps extends LazyLogging {

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
    *
    * @param selfishAgentExperienceFile
    * @param optimalAgentExperienceFile
    * @return
    */
  def computePerformanceMetrics(selfishAgentExperienceFile: File, optimalAgentExperienceFile: File): Either[Exception, PerformanceMetrics] = {
    Try {
      case class AgentExperienceRow(
        agentId: String,
        requestClass: String,
        departureTime: Int,
        travelTime: Double,
        distance: Double,
        replannings: Int
      )
      implicit val dec: HeaderDecoder[AgentExperienceRow] =
        HeaderDecoder.decoder(
          "agentId",
          "requestClass",
          "departureTime",
          "travelTime",
          "distance",
          "replannings"
        ) { AgentExperienceRow.apply }

      // pull in all data (these unsafe readers can throw)
      val selfishRows: Map[String, AgentExperienceRow] =
        selfishAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row =>
            s"${row.agentId}#${row.departureTime}" -> row
          }
          .toMap
      val optimalRows: Map[String, AgentExperienceRow] =
        optimalAgentExperienceFile
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
                acc.copy(
                  ttDiffMin = acc.ttDiffMin + (optimalRow.travelTime - selfishRow.travelTime) / 60.0,
                  ttDiffNorm = acc.ttDiffNorm + (selfishRow.travelTime - optimalRow.travelTime) / selfishRow.travelTime,
                  distDiffMiles = acc.distDiffMiles + (optimalRow.distance - selfishRow.distance) / 1609.0,
                  distDiffNorm = acc.distDiffNorm + (selfishRow.distance - optimalRow.distance) / selfishRow.distance,
                  speedDiffMph = acc.speedDiffMph + math.min((optimalRow.travelTime - selfishRow.travelTime) * 3600 / 1609, SpeedUpperBoundMph),
                  speedDiffNorm = (selfishRow.travelTime - optimalRow.travelTime) / selfishRow.travelTime,
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
