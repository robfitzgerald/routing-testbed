package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{EdgeBPR, EdgeBPRUpdateOps}
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond

sealed trait EdgeUpdateFunctionConfig {
  // todo:
  //  - implement as ADT w/ extension method
  //  - return IO[EdgeBPR]
  //    - refactor LocalAdjacencyListFlowNetwork.updateEdgeFlows to traverse these updates
  def build(): (EdgeBPR, Option[Flow], MetersPerSecond) => EdgeBPR
}

object EdgeUpdateFunctionConfig {

  import EdgeBPRUpdateOps._

  case object FlowCount extends EdgeUpdateFunctionConfig {
    def build(): (EdgeBPR, Option[Flow], MetersPerSecond) => EdgeBPR = withSpeedUpdate(edgeUpdateWithFlowCount)
  }

  case class FlowRate(bufferTime: SimTime) extends EdgeUpdateFunctionConfig {
    require(bufferTime > SimTime.Zero, "bufferTime must be positive-valued")
    throw new NotImplementedError("deprecated, please refactor for incoming flows are no longer marginal but absolute")

    def build(): (EdgeBPR, Option[Flow], MetersPerSecond) => EdgeBPR =
      withSpeedUpdate(edgeUpdateWithFlowRate(bufferTime))
  }

  case class MarginalFlowAndDecay(decay: Flow, epsilon: Flow) extends EdgeUpdateFunctionConfig {

    throw new NotImplementedError("deprecated, please refactor for incoming flows are no longer marginal but absolute")

    def build(): (EdgeBPR, Option[Flow], MetersPerSecond) => EdgeBPR =
      withSpeedUpdate(edgeUpdateWithMarginalFlowAndDecay(decay, epsilon))
  }
}
