package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.time.LocalTime

import scala.util.Try

import kantan.csv._

case class BatchDataRow(
  time: LocalTime,
  batch_id: String,
  batch_size: Int,
  avgAltPathsPerAgent: Double,
  searchSpaceSamples: BigDecimal,
  searchSpaceSize: BigDecimal,
  searchSpaceExplored: Percent,
  soAssignmentPercent: Percent,
)

object BatchDataRow {

  implicit val timeCellDecoder: CellDecoder[LocalTime] =
    CellDecoder.from { timeString =>
      Try { LocalTime.parse(timeString) }.toEither.left.map { t =>
        DecodeError.TypeError(t)
      }
    }

  implicit val percentDecoder: CellDecoder[Percent] = Percent.cellDecoder

  val headerDecoder: HeaderDecoder[BatchDataRow] =
    HeaderDecoder.decoder(
      "time",
      "batch_id",
      "batch_size",
      "avgAltPathsPerAgent",
      "searchSpaceSamples",
      "searchSpaceSize",
      "searchSpaceExplored",
      "soAssignmentPercent"
    ) { BatchDataRow.apply }
}
