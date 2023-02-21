package edu.colorado.fitzgero.sotestbed.config

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.routing.FlowObservationOps
import edu.colorado.fitzgero.sotestbed.algorithm.routing.FlowObservationOps.PathToMarginalFlows

sealed trait PathToMarginalFlowsFunctionConfig {
  def build[F[_]: Monad, V, E](): FlowObservationOps.PathToMarginalFlows[F, V, E]
}

object PathToMarginalFlowsFunctionConfig {

  final case object Default extends PathToMarginalFlowsFunctionConfig {
    def build[F[_]: Monad, V, E](): PathToMarginalFlows[F, V, E] = FlowObservationOps.defaultMarginalFlow
  }

  // ...
  final case class LinkDecay(
    rate: Double
  ) extends PathToMarginalFlowsFunctionConfig {
    def build[F[_]: Monad, V, E](): PathToMarginalFlows[F, V, E] = ???
  }
}
