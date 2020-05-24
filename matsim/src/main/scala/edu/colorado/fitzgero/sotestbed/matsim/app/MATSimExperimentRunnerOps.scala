package edu.colorado.fitzgero.sotestbed.matsim.app

import scala.util.Try

import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.util.SummaryStats
import kantan.csv._
import kantan.csv.ops._

object MATSimExperimentRunnerOps {

  def fairness(config: MATSimRunConfig): Either[Exception, SummaryStats] = {
    Try {
      val selfishConfig =
        config.copy(
          scenarioData = config.scenarioData.copy(
            algorithm = "selfish",
          )
        )
      val selfishAgentExperienceFile =
        selfishConfig.experimentLoggingDirectory
          .resolve("agentExperience.csv")
          .toFile
      val optimalAgentExperienceFile =
        config.experimentLoggingDirectory
          .resolve("agentExperience.csv")
          .toFile

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

      // pull in all data
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
          .flatMap { row =>
            if (row.requestClass == "ue") None
            else Some { s"${row.agentId}#${row.departureTime}" -> row }
          }
          .toMap

      val diffs: Iterable[Double] = for {
        (agentTimeIndex, optimalRow) <- optimalRows
        selfishRow                   <- selfishRows.get(agentTimeIndex)
      } yield {
        selfishRow.travelTime - optimalRow.travelTime
      }

      val summaryStats = diffs.foldLeft(SummaryStats()) { _.add(_) }

      summaryStats

    }.toEither.left.map { t =>
      new Exception(t)
    }
  }
}
