package edu.colorado.fitzgero.sotestbed.model.numeric

final class RunTime(val value: Double) extends AnyVal {
  def +(that: RunTime): RunTime = RunTime(this.value + that.value)
  def -(that: RunTime): RunTime = RunTime(this.value - that.value)
  def <(that: Double): Boolean  = this.value < that
  def <=(that: Double): Boolean = this.value < that
  def >(that: Double): Boolean  = this.value > that
  def >=(that: Double): Boolean = this.value >= that
  override def toString: String = this.value.toString
}

object RunTime {
  def apply(value: Double): RunTime = new RunTime(value)
  val Zero: RunTime                 = new RunTime(0.0)
  val Now: RunTime                  = RunTime(System.currentTimeMillis)
}
