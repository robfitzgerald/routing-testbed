package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.population

import java.time.LocalTime
import kantan.csv._
import scala.util.Try
import java.time.format.DateTimeFormatter

final case class DemandTableRow(src: String, dst: String, bin: String, start: LocalTime, end: LocalTime, cnt: Int)

object DemandTableRow {

  val TimeFormat = "HH:mm:ss"

  private implicit val timeOrIntDecoder: CellDecoder[LocalTime] =
    CellDecoder.from { timeString =>
      Try { LocalTime.ofSecondOfDay(timeString.toLong) }
        .orElse { Try { LocalTime.parse(timeString, DateTimeFormatter.ofPattern(TimeFormat)) } }
        .toEither
        .left
        .map { t =>
          DecodeError.TypeError(
            s"could not parse '$timeString' as int (second of day) or LocalTime in $TimeFormat format"
          )
        }
    }

  def headerDecoder(
    srcCol: String,
    dstCol: String,
    binNameCol: String,
    startCol: String,
    endCol: String,
    cntCol: String
  ): HeaderDecoder[DemandTableRow] =
    HeaderDecoder.decoder(srcCol, dstCol, binNameCol, startCol, endCol, cntCol) { DemandTableRow.apply }
}
