package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._

final class SimTime(val value: Long) extends AnyVal {
  def + (that: SimTime): SimTime = SimTime(this.value + that.value)
  def - (that: SimTime): SimTime = SimTime(this.value - that.value)
  def < (that: SimTime): Boolean = this.value < that.value
  def <= (that: SimTime): Boolean = this.value <= that.value
  def > (that: SimTime): Boolean = this.value > that.value
  def >= (that: SimTime): Boolean = this.value >= that.value
}

object SimTime {
  val Zero: SimTime = new SimTime(0)
  val EndOfDay: SimTime = new SimTime(172800) // two days of time as a buffer for any sim events
  def apply[T: Numeric](time: T): SimTime = new SimTime(time.toLong)
}