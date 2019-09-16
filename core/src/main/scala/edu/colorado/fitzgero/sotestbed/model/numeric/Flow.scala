package edu.colorado.fitzgero.sotestbed.model.numeric

final class Flow (val value: Double) extends AnyVal {
  def + (that: Flow): Flow = Flow(this.value + that.value)
  def * (that: Flow): Flow = Flow(this.value * that.value)
  def > (that: Flow): Boolean = this.value > that.value
  def < (that: Flow): Boolean = this.value < that.value
}

object Flow {
  def apply(value: Double): Flow = new Flow(value)
  val Zero: Flow = new Flow(0.0)
}
