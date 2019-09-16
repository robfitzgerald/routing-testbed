package edu.colorado.fitzgero.sotestbed.model.numeric

class Meters (val value: Double) extends AnyVal {
  def / (that: MetersPerSecond): TravelTimeSeconds = TravelTimeSeconds(this.value / that.value)
}

object Meters {
  def apply(value: Double): Meters = new Meters(math.max(0.0, value))
  val Zero: Meters = new Meters(0.0)
}
