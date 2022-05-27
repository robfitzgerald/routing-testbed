package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Try

import kantan.csv.{CellDecoder, DecodeError, DecodeResult}

final case class Karma(value: Long) extends AnyVal {
  override def toString: String = value.toString
}

object Karma {

  val Zero: Karma = Karma(0)

  implicit val numeric: Numeric[Karma] = new Numeric[Karma] {
    def plus(x: Karma, y: Karma): Karma         = Karma(x.value + y.value)
    def minus(x: Karma, y: Karma): Karma        = Karma(x.value - y.value)
    def times(x: Karma, y: Karma): Karma        = Karma(x.value * y.value)
    def negate(x: Karma): Karma                 = Karma(-x.value)
    def fromInt(x: Int): Karma                  = Karma(x.toLong)
    def parseString(str: String): Option[Karma] = Try(Karma(str.toLong)).toOption
    def toInt(x: Karma): Int                    = x.value.toInt
    def toLong(x: Karma): Long                  = x.value
    def toFloat(x: Karma): Float                = x.value.toFloat
    def toDouble(x: Karma): Double              = x.value.toDouble
    def compare(x: Karma, y: Karma): Int        = x.value.compare(y.value)
  }

  implicit val cd: CellDecoder[Karma] = CellDecoder[Long].emap { s => DecodeResult(Karma(s)) }
}
