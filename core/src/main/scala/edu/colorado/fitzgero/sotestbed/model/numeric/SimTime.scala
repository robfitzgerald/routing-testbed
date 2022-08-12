package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._
import scala.collection.immutable.NumericRange

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

  val EndOfDay: SimTime                   = new SimTime(172800) // two days of time as a buffer for any sim events
  def apply[T: Numeric](time: T): SimTime = new SimTime(time.toLong)

  implicit val cd: CellDecoder[SimTime] = CellDecoder[Long].emap {
    case n if n < 0L => Left(DecodeError.TypeError(s"sim time cannot be less than 0, but found $n"))
    case n           => Right(SimTime(n))
  }
}
