package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}

object EdgeBPRCostOps {

  def capacityObservation: EdgeBPR => Flow => Cost = {
    edge: EdgeBPR =>
      marginalFlow: Flow =>
    Cost((edge.flow.value + marginalFlow.value)  / edge.capacity.value)
  }

  // based on default values from literature
  val BPRCostFunctionWithBookCoefficients: EdgeBPR => Cost = costFunction(0.15, 4.0)

  def costFunction(alpha: Double, beta: Double): EdgeBPR => Cost = { edge: EdgeBPR =>
    {
      marginalCostFunction(alpha, beta)(edge)(edge.flow)
    }
  }

  def freeFlowCostFunction: EdgeBPR => Cost = { edge: EdgeBPR =>
    {
      Cost(edge.freeFlowTravelTime.value)
    }
  }

  def marginalCostFunction(alpha: Double, beta: Double): EdgeBPR => Flow => Cost = { edge: EdgeBPR =>
    { flow: Flow =>
      {
        val term1    = edge.freeFlowTravelTime.value
        val term2    = alpha * edge.freeFlowTravelTime.value
        val term3    = math.pow((flow.value + edge.flow.value) / edge.capacity.value, beta)
        val costFlow = Cost(term1 + term2 * term3)
        costFlow
      }
    }
  }
}
