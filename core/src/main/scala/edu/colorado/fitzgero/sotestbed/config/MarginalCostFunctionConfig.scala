package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRCostOps}

sealed trait MarginalCostFunctionConfig {
  def build(): EdgeBPR => Flow => Cost
}

object MarginalCostFunctionConfig {
  final case class EdgeBPRFunction(
    alpha: Double,
    beta: Double
  ) extends MarginalCostFunctionConfig {
    def build(): EdgeBPR => Flow => Cost = EdgeBPRCostOps.marginalCostFunction(alpha, beta)
  }
}
