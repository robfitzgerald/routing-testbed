package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingOps
import edu.colorado.fitzgero.sotestbed.model.numeric.Flow

sealed trait CombineFlowsFunctionConfig {
  def build(): Iterable[Flow] => Flow
}

object CombineFlowsFunctionConfig {
  final case object Sum extends CombineFlowsFunctionConfig {
    def build(): Iterable[Flow] => Flow = RoutingOps.defaultCombineFlows
  }
}
