package edu.colorado.fitzgero.sotestbed.model.numeric


class TravelTimeSeconds (val value: Double) extends AnyVal {
  def - (that: TravelTimeSeconds): TravelTimeSeconds = TravelTimeSeconds(this.value - that.value)
  def + (that: TravelTimeSeconds): TravelTimeSeconds = TravelTimeSeconds(this.value + that.value)
  def <= (that: TravelTimeSeconds): Boolean = this.value <= that.value
  def < (that: TravelTimeSeconds): Boolean = this.value < that.value
}

object TravelTimeSeconds {
  def apply(value: Double): TravelTimeSeconds = new TravelTimeSeconds(math.max(0.0, value))
  val Zero: TravelTimeSeconds = new TravelTimeSeconds(0.0)
}
