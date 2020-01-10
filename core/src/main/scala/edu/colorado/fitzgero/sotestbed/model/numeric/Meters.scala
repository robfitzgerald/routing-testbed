package edu.colorado.fitzgero.sotestbed.model.numeric

class Meters (val value: Double) extends AnyVal {
  def / (that: Meters): Meters = Meters(this.value / that.value)
  def * (that: Double): Meters = new Meters(this.value * that)
  def < (that: Meters): Boolean = this.value < that.value
  override def toString: String = f"${this.value}%.2f"
}

object Meters {
  def apply(value: Double): Meters = new Meters(math.max(0.0, value))
  def toTravelTime (lhs: Meters, rhs: MetersPerSecond): TravelTimeSeconds = TravelTimeSeconds(lhs.value / rhs.value)
  val Zero: Meters = new Meters(0.0)
}
