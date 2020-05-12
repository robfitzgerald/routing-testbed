package edu.colorado.fitzgero.sotestbed.model.numeric

import scala.Numeric.Implicits._

final class Cost(val value: Double) extends AnyVal {
  def <(that: Cost): Boolean    = this.value < that.value
  def <=(that: Cost): Boolean   = this.value <= that.value
  def >=(that: Cost): Boolean   = this.value >= that.value
  def +(that: Cost): Cost       = Cost(this.value + that.value)
  def -(that: Cost): Cost       = Cost(this.value - that.value)
  def /(that: Cost): Cost       = Cost(this.value / that.value)
  override def toString: String = f"${this.value}%.2f"
}

object Cost {
  def apply(value: Double): Cost       = new Cost(value)
  def Zero: Cost                       = new Cost(0.0)
  def PositiveInfinity: Cost           = new Cost(Double.PositiveInfinity)
  def apply[C: Numeric](cost: C): Cost = new Cost(cost.toDouble)
  implicit val CostNumeric: Numeric[Cost] = new Numeric[Cost] {
    def plus(x: Cost, y: Cost): Cost = Cost(x.value + y.value)

    def +(x: Cost, y: Cost): Cost = Cost(x.value + y.value)

    def minus(x: Cost, y: Cost): Cost = Cost(x.value - y.value)

    def -(x: Cost, y: Cost): Cost = Cost(x.value - y.value)

    def times(x: Cost, y: Cost): Cost = Cost(x.value * y.value)

    def *(x: Cost, y: Cost): Cost = Cost(x.value * y.value)

    def negate(x: Cost): Cost = Cost(-x.value)

    def -(x: Cost): Cost = Cost(-x.value)

    def fromInt(x: Int): Cost = Cost(x.toDouble)

    def toInt(x: Cost): Int = x.value.toInt

    def toLong(x: Cost): Long = x.value.toLong

    def toFloat(x: Cost): Float = x.value.toFloat

    def toDouble(x: Cost): Double = x.value

    def compare(x: Cost, y: Cost): Int = (x.value * 1000).toInt - (y.value * 1000).toInt
  }
  implicit val CostOrdering: Ordering[Cost] = Ordering.by(_.value)
}
