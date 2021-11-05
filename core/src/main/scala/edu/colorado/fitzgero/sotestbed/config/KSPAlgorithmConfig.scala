package edu.colorado.fitzgero.sotestbed.config

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.kSPwLO_SVP_Sync
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

sealed trait KSPAlgorithmConfig {
  def k: Int
  def build[F[_]: Monad, V, E](): altpaths.KSPAlgorithm[F, V, E]
  def kspTerminationFunction: KSPTerminationFunctionConfig
}

object KSPAlgorithmConfig {
  final case class SvpLoSync(
    k: Int,
    theta: Cost,
    minBatchSize: Int,
    kspTerminationFunction: KSPTerminationFunctionConfig
  ) extends KSPAlgorithmConfig {

    require(Cost.Zero <= theta || theta <= Cost(1), "KSPAlgorithm.theta must be between zero and one")

    override def build[F[_]: Monad, V, E](): altpaths.KSPAlgorithm[F, V, E] =
      new kSPwLO_SVP_Sync(
        k = k,
        theta = theta,
        terminationFunction = kspTerminationFunction.build(),
        minBatchSize = minBatchSize
      )
  }
}
