package edu.colorado.fitzgero.sotestbed.model.numeric

final class Cost (val value: Double) extends AnyVal
object Cost {
  def apply(value: Double): Cost = new Cost(value)
  def Zero: Cost = new Cost(0.0)
  implicit val CostOrdering: Ordering[Cost] = Ordering.by(_.value)
}