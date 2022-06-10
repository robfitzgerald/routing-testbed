package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRCostOps}

sealed trait MarginalCostFunctionConfig

object MarginalCostFunctionConfig {

  final case class EdgeBPRFunction(alpha: Double, beta: Double) extends MarginalCostFunctionConfig

  case object CapacityCostFunction extends MarginalCostFunctionConfig

  implicit class CostFnExtension(cfn: MarginalCostFunctionConfig) {

    def build(): EdgeBPR => Flow => Cost = cfn match {
      case EdgeBPRFunction(alpha, beta) => EdgeBPRCostOps.marginalCostFunction(alpha, beta)
      case CapacityCostFunction         => EdgeBPRCostOps.capacityObservation
    }
  }
}
