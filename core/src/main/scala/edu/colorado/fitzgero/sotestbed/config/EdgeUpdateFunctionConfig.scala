package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}

sealed trait EdgeUpdateFunctionConfig {
  // todo:
  //  - implement as ADT w/ extension method
  //  - return IO[EdgeBPR]
  //    - refactor LocalAdjacencyListFlowNetwork.updateEdgeFlows to traverse these updates
  def build(): (EdgeBPR, Flow) => EdgeBPR
}

object EdgeUpdateFunctionConfig {

  final case object FlowCount extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowCount
  }

  final case class FlowRate(bufferTime: SimTime) extends EdgeUpdateFunctionConfig {
    require(bufferTime > SimTime.Zero, "bufferTime must be positive-valued")
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithFlowRate(bufferTime)
  }

  final case class MarginalFlowAndDecay(decay: Flow, epsilon: Flow) extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Flow) => EdgeBPR = EdgeBPRUpdateOps.edgeUpdateWithMarginalFlowAndDecay(decay, epsilon)
  }
}
