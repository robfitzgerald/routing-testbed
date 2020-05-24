package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}

sealed trait EdgeUpdateFunctionConfig {
  def build(): (EdgeBPR, Flow) => EdgeBPR
}

object EdgeUpdateFunctionConfig {
  final case object FlowCount extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowCount
  }

  final case object FlowCountDelta extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowCountDelta
  }

  final case class FlowRate(bufferTime: SimTime) extends EdgeUpdateFunctionConfig {
    require(bufferTime > SimTime.Zero, "bufferTime must be positive-valued")
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowRate(bufferTime)
  }

  final case class MarginalFlowAndDecay(decay: Flow, epsilon: Flow) extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithMarginalFlowAndDecay(decay, epsilon)
  }
}
