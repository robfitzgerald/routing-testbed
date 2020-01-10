package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

sealed trait MarginalCostFunctionConfig {
  def build(): edge.EdgeBPR => Flow => Cost
}
object MarginalCostFunctionConfig {
  final case class EdgeBPR(
    alpha: Double,
    beta: Double
  ) extends MarginalCostFunctionConfig {
    def build(): edge.EdgeBPR => Flow => Cost = edge.EdgeBPRCostOps.marginalCostFunction(alpha, beta)
  }
}
