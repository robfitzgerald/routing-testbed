package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingOps
import edu.colorado.fitzgero.sotestbed.model.numeric.Flow

sealed trait CombineFlowsFunction {
  def build(): Iterable[Flow] => Flow
}
object CombineFlowsFunction {
  final case object Sum extends CombineFlowsFunction {
    def build(): Iterable[Flow] => Flow = RoutingOps.defaultCombineFlows
  }
}
