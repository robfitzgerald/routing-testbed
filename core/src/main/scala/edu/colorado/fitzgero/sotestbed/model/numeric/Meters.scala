package edu.colorado.fitzgero.sotestbed.model.numeric

import kantan.csv._

class Meters(val value: Double) extends AnyVal {
  def +(that: Meters): Meters   = Meters(this.value + that.value)
  def /(that: Meters): Meters   = Meters(this.value / that.value)
  def *(that: Double): Meters   = new Meters(this.value * that)
  def <(that: Meters): Boolean  = this.value < that.value
  override def toString: String = f"${this.value}%.2f"
}

object Meters {
  def apply(value: Double): Meters                                       = new Meters(math.max(0.0, value))
  def toTravelTime(lhs: Meters, rhs: MetersPerSecond): TravelTimeSeconds = TravelTimeSeconds(lhs.value / rhs.value)
  val Zero: Meters                                                       = new Meters(0.0)

  implicit val cd: CellDecoder[Meters] = CellDecoder[Double].emap {
    case m if m < 0.0 => Left(DecodeError.TypeError(s"meters cannot be negative, but found $m"))
    case m            => Right(Meters(m))
  }
}
