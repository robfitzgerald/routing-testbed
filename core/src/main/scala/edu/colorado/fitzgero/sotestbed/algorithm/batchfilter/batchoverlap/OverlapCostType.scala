package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

sealed trait OverlapCostType {
  def cost(overlapCost: Cost, totalCost: Cost): Double
  def aggregate(overlapValues: Iterable[Double]): Double
  def print(cost: Double): String
}

object OverlapCostType {

  final case object Normalized extends OverlapCostType {

    def cost(overlapCost: Cost, totalCost: Cost): Double =
      if (totalCost == Cost.Zero) 0.0 else overlapCost.value / totalCost.value

    def aggregate(overlapValues: Iterable[Double]): Double =
      if (overlapValues.isEmpty) 0.0 else overlapValues.sum / overlapValues.size

    override def print(cost: Double): String = f"${cost * 100.0}%.2f%%"
  }

  final case object NonNormalized extends OverlapCostType {
    def cost(overlapCost: Cost, totalCost: Cost): Double = overlapCost.value

    def aggregate(overlapValues: Iterable[Double]): Double = overlapValues.fold(0.0) { _ + _ }

    override def print(cost: Double): String = f"$cost%.0f"
  }
}
