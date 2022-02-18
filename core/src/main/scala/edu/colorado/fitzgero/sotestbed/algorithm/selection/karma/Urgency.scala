package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import kantan.csv.{CellDecoder, DecodeResult}

final case class Urgency(value: Double) extends AnyVal {
  override def toString: String = value.toString
}

object Urgency {

  implicit val cd: CellDecoder[Urgency] = CellDecoder[Double].emap { s => DecodeResult(Urgency(s)) }
}
