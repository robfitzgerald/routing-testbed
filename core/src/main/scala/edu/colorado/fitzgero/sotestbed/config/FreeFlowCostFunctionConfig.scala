package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

sealed trait FreeFlowCostFunctionConfig

object FreeFlowCostFunctionConfig {

  /**
    * for capacity-based cost functions, depict 'free flow' as Zero
    */
  case object CapacityBased extends FreeFlowCostFunctionConfig

  /**
    * for travel time-based cost functions, depict 'free flow' as
    * distance / speed.
    */
  case object TravelTimeBased extends FreeFlowCostFunctionConfig

  implicit class FreeFlowExtensions(ff: FreeFlowCostFunctionConfig) {

    def getFreeFlow(edge: EdgeBPR): Cost = ff match {
      case CapacityBased   => Cost.Zero
      case TravelTimeBased => Cost(edge.freeFlowTravelTime.value)
    }
  }
}
