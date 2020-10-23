package edu.colorado.fitzgero.sotestbed.matsim.analysis

import scala.util.Try

import kantan.csv.{CellDecoder, DecodeError}

case class Percent(value: Double) extends AnyVal

object Percent {

  val cellDecoder: CellDecoder[Percent] =
    CellDecoder.from { string =>
      Try {
        val cleanedString        = string.replace("%", "")
        val percentValue: Double = cleanedString.toDouble / 100.0
        Percent(percentValue)
      }.toEither.left.map { t =>
        DecodeError.TypeError(t)
      }
    }
}
