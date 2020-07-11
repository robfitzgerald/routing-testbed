package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File

import scala.util.Try

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import kantan.csv._
import kantan.csv.ops._

case class OverallMetrics(
  travelTimeMinutes: Double = 0.0,
  distanceMiles: Double = 0.0,
  speedMph: Double = 0.0,
  count: Int = 0
) {

  def add(row: AgentExperienceRow): OverallMetrics = {
    val rowDistMiles         = row.distance / 1609.0
    val rowTravelTimeMinutes = row.travelTime / 60.0
    val rowSpeedMph          = (row.distance / row.travelTime) * (3600.0 / 1609.0)
    this.copy(
      travelTimeMinutes = this.travelTimeMinutes + rowTravelTimeMinutes,
      distanceMiles = this.distanceMiles + rowDistMiles,
      speedMph = this.speedMph + rowSpeedMph,
      count = this.count + 1
    )
  }

  def avg: OverallMetrics = this.copy(
    distanceMiles = this.distanceMiles / this.count,
    speedMph = this.speedMph / this.count,
    travelTimeMinutes = this.travelTimeMinutes / this.count,
    count = this.count
  )

  override def toString: String = f"${this.travelTimeMinutes}%.2f,${this.distanceMiles}%.2f,${this.speedMph}%.2f"
}

object OverallMetrics {

  val Header = "avgTravelTimeMinutes,avgDistanceMiles,avgSpeedMph"

  /**
    * constructs overall metrics from a matsim run configuration
    * @param config the config of a simulation which has run
    * @return either an exception, or, the metrics
    */
  def apply(config: MATSimRunConfig): Either[Exception, OverallMetrics] = {
    apply(config.experimentLoggingDirectory.resolve("agentExperience.csv").toFile)
  }

  /**
    * given a [[File]] with agent experience stats, compute overall metrics of a simulation run
    * @param agentExperienceFile the agentExperience.csv file
    * @return either an exception, or, the metrics
    */
  def apply(agentExperienceFile: File): Either[Exception, OverallMetrics] = {
    Try {

      implicit val dec: HeaderDecoder[AgentExperienceRow] = AgentExperienceRow.headerDecoder

      // pull in all data (these unsafe readers can throw)
      val agentExperienceRows: Map[String, AgentExperienceRow] =
        agentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row =>
            s"${row.agentId}#${row.departureTime}" -> row
          }
          .toMap

      // accumulate the rows into a summation
      val overallMetrics: OverallMetrics =
        agentExperienceRows.foldLeft(OverallMetrics()) {
          case (acc, (_, row)) => acc.add(row)
        }

      // average the summed values
      val avgOverallMetrics = overallMetrics.avg

      avgOverallMetrics

    }.toEither.left.map { t =>
      new Exception(t)
    }
  }
}