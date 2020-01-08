package edu.colorado.fitzgero.sotestbed.model.numeric

class MetersPerSecond (val value: Double) extends AnyVal {
  override def toString: String = f"${this.value}%.2f"
}

object MetersPerSecond {
  def apply(value: Double): MetersPerSecond = new MetersPerSecond(math.max(0.0, value))
  val Zero: MetersPerSecond = new MetersPerSecond(0.0)
  def apply(meters: Meters, seconds: TravelTimeSeconds): MetersPerSecond = new MetersPerSecond(meters.value / seconds.value)
}