package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._

import cats.data.Validated

final class SimTime(val value: Long) extends AnyVal {
  def + (that: SimTime): SimTime = SimTime(this.value + that.value)
  def - (that: SimTime): SimTime = SimTime(this.value - that.value)
  def * (that: SimTime): SimTime = SimTime(this.value * that.value)
  def < (that: SimTime): Boolean = this.value < that.value
  def <= (that: SimTime): Boolean = this.value <= that.value
  def > (that: SimTime): Boolean = this.value > that.value
  def >= (that: SimTime): Boolean = this.value >= that.value
  def toHourOfDay: Long = this.value / 3600
  def toMinuteOfHour: Long = (this.value % 3600) / 60


  override def toString: String = {
    def padLeft(n: String): String = if (n.length == 1) s"0$n" else n
    val hour: String =
      padLeft((this.value / 3600).toString)
    val min: String = padLeft(((this.value % 3600) / 60).toString)
    s"$hour:$min"
  }
}

object SimTime {
  val Zero: SimTime = new SimTime(0)
  val StartOfDay: SimTime = SimTime.Zero
  // used by MATSim to flag time before the first time of the day
  val Infinity: SimTime = SimTime(Double.PositiveInfinity)
  val Minute: SimTime = SimTime(60)
  val Hour: SimTime = Minute * SimTime(60)
  def hour(h: Int): SimTime = Hour * SimTime(h)

  val EndOfDay: SimTime = new SimTime(172800) // two days of time as a buffer for any sim events
  def apply[T: Numeric](time: T): SimTime = new SimTime(time.toLong)
}