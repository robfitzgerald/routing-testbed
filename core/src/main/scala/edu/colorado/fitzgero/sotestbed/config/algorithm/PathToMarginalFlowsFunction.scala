package edu.colorado.fitzgero.sotestbed.config.algorithm

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingOps
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingOps.PathToMarginalFlows

sealed trait PathToMarginalFlowsFunction {
  def build[F[_]: Monad, V, E](): RoutingOps.PathToMarginalFlows[F, V, E]
}
object PathToMarginalFlowsFunction {
  final case object Default extends PathToMarginalFlowsFunction {
    def build[F[_] : Monad, V, E](): PathToMarginalFlows[F, V, E] = RoutingOps.defaultMarginalFlow
  }

  // ...
  final case class LinkDecay(
    rate: Double
  ) extends PathToMarginalFlowsFunction {
    def build[F[_] : Monad, V, E](): PathToMarginalFlows[F, V, E] = ???
  }
}
