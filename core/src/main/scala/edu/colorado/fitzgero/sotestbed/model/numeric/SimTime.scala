package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._
import scala.collection.immutable.NumericRange
import java.time.LocalTime
import scala.util.Try

import cats.data.Validated
import kantan.csv._

final class SimTime(val value: Long) extends AnyVal {
  def +(that: SimTime): SimTime                          = SimTime(this.value + that.value)
  def -(that: SimTime): SimTime                          = SimTime(this.value - that.value)
  def *(that: SimTime): SimTime                          = SimTime(this.value * that.value)
  def /(that: SimTime): SimTime                          = SimTime(this.value / that.value)
  def %(that: SimTime): SimTime                          = SimTime(this.value % that.value)
  def <(that: SimTime): Boolean                          = this.value < that.value
  def <=(that: SimTime): Boolean                         = this.value <= that.value
  def >(that: SimTime): Boolean                          = this.value > that.value
  def >=(that: SimTime): Boolean                         = this.value >= that.value
  def unary_-()                                          = -this.value
  def to(that: SimTime): NumericRange.Inclusive[Long]    = this.value to that.value
  def until(that: SimTime): NumericRange.Exclusive[Long] = this.value until that.value
  def toHourOfDay: Long                                  = this.value / 3600
  def toMinuteOfHour: Long                               = (this.value % 3600) / 60

  override def toString: String = {
    def padLeft(n: String): String = if (n.length == 1) s"0$n" else n
    val hour: String =
      padLeft((this.value / 3600).toString)
    val min: String = padLeft(((this.value % 3600) / 60).toString)
    val sec: String = padLeft((this.value  % 60).toString)
    s"$hour:$min:$sec"
  }
}

object SimTime {
  val Zero: SimTime       = new SimTime(0)
  val StartOfDay: SimTime = SimTime.Zero
  // used by MATSim to flag time before the first time of the day
  val Infinity: SimTime       = SimTime(Double.PositiveInfinity)
  val Minute: SimTime         = SimTime(60)
  val Hour: SimTime           = Minute * SimTime(60)
  def minute(m: Int): SimTime = SimTime(m) * Minute
  def hour(h: Int): SimTime   = SimTime(h) * Hour

  val EndOfDay: SimTime                            = new SimTime(172800) // two days of time as a buffer for any sim events
  def apply[T: Numeric](time: T): SimTime          = new SimTime(time.toLong)
  def fromLocalTime(localTime: LocalTime): SimTime = SimTime(localTime.toSecondOfDay)

  /**
    * accepts a string in any of these formats and parses them as SimTime
    * (listed here as regex patterns)
    *
    * s+
    * m+:s+
    * h+:m+:s+
    *
    * note: places no restriction on time value sizes
    *
    * @param s string to decode as SimTime value
    * @return either a SimTime or an error
    */
  def fromString(s: String): Either[Error, SimTime] = {
    val digitBins = s.split(":")
    val parseAndDecodeResult = digitBins.toList match {
      case sStr :: Nil =>
        Try { SimTime(sStr.toInt) }.toEither
      case mStr :: sStr :: Nil =>
        Try {
          SimTime.minute(mStr.toInt) + SimTime(sStr.toInt)
        }.toEither
      case hStr :: mStr :: sStr :: Nil =>
        Try {
          SimTime.hour(hStr.toInt) +
            SimTime.minute(mStr.toInt) +
            SimTime(sStr.toInt)
        }.toEither
      case other =>
        Left(new Error(f"unable to parse time with more than 2 colons from $other"))
    }
    parseAndDecodeResult.left.map { t => new Error(f"failure parsing SimTime from string $s", t) }
  }

  val cd: CellDecoder[SimTime] = CellDecoder.from { s =>
    SimTime.fromString(s) match {
      case Left(value) =>
        val err = DecodeError.TypeError(f"failure parsing SimTime: ${value.getMessage}")
        Left(err)
      case Right(value) =>
        Right(value)
    }
  }

  val ce: CellEncoder[SimTime] = CellEncoder.from { _.toString }

  implicit val codec: CellCodec[SimTime] = CellCodec.from(cd, ce)

}
