package edu.colorado.fitzgero.sotestbed.model.numeric

case class MetersPerSecond(val value: Double) extends AnyVal {
  override def toString: String = f"${this.value}%.2f"
  def asMph: Double             = value * MetersPerSecond.secondsPerHourOverMetersPerMile

  def isInfinite: Boolean =
    value == Double.PositiveInfinity || value == Double.NegativeInfinity
}

object MetersPerSecond {
  val secondsPerHourOverMetersPerMile: Double = 3600.0 / 1609.0
  def apply(value: Double): MetersPerSecond   = new MetersPerSecond(math.max(0.0, value))
  val Zero: MetersPerSecond                   = MetersPerSecond(0.0)

  def apply(meters: Meters, seconds: TravelTimeSeconds): MetersPerSecond =
    new MetersPerSecond(meters.value / seconds.value)
}
