package edu.colorado.fitzgero.sotestbed.model.numeric

final class RunTime(val value: Double) extends AnyVal {
  def + (that: RunTime): RunTime = RunTime(this.value + that.value)
}

object RunTime {
  def apply(value: Double): RunTime = new RunTime(value)
  val Zero: RunTime = new RunTime(0.0)
}