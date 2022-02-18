package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Try

import kantan.csv.{CellDecoder, DecodeError, DecodeResult}

final case class Karma(value: Double) extends AnyVal {
  override def toString: String = value.toString
}

object Karma {

  implicit val numeric: Numeric[Karma] = new Numeric[Karma] {
    def plus(x: Karma, y: Karma): Karma         = Karma(x.value + y.value)
    def minus(x: Karma, y: Karma): Karma        = Karma(x.value - y.value)
    def times(x: Karma, y: Karma): Karma        = Karma(x.value * y.value)
    def negate(x: Karma): Karma                 = Karma(-x.value)
    def fromInt(x: Int): Karma                  = Karma(x.toDouble)
    def parseString(str: String): Option[Karma] = Try(Karma(str.toDouble)).toOption
    def toInt(x: Karma): Int                    = x.value.toInt
    def toLong(x: Karma): Long                  = x.value.toLong
    def toFloat(x: Karma): Float                = x.value.toFloat
    def toDouble(x: Karma): Double              = x.value
    def compare(x: Karma, y: Karma): Int        = x.value.compare(y.value)
  }

  implicit val cd: CellDecoder[Karma] = CellDecoder[Double].emap { s => DecodeResult(Karma(s)) }
}
