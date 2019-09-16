package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

object EdgeBPRCostOps {
  // based on default values from literature
  val BPRCostFunctionWithBookCoefficients: EdgeBPR => Cost = costFunction(0.15, 4.0)

  def costFunction(alpha: Double, beta: Double): EdgeBPR => Cost = {
    edge: EdgeBPR => {
      val term1 = edge.freeFlowTravelTime.value
      val term2 = alpha * edge.freeFlowTravelTime.value
      val term3 = math.pow(edge.flow.value / edge.capacity.value, beta)
      Cost(term1 + term2 * term3)
    }
  }

  def freeFlowCostFunction: EdgeBPR => Cost = {
    edge: EdgeBPR => {
      Cost(edge.freeFlowTravelTime.value)
    }
  }
}
