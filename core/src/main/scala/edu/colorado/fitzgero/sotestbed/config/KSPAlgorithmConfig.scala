package edu.colorado.fitzgero.sotestbed.config

import cats.Monad
import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.kSPwLO_SVP_Sync
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait KSPAlgorithmConfig {
  def k: Int
  def build(): altpaths.KSPAlgorithm
  def kspTerminationFunction: KSPTerminationFunctionConfig
}

object KSPAlgorithmConfig {

  final case class SvpLoSync(
    k: Int,
    theta: Cost,
    minBatchSize: Int,
    kspTerminationFunction: KSPTerminationFunctionConfig,
    marginalCostFunction: MarginalCostFunctionConfig
  ) extends KSPAlgorithmConfig {

    require(Cost.Zero <= theta || theta <= Cost(1), "KSPAlgorithm.theta must be between zero and one")

    override def build(): altpaths.KSPAlgorithm = {
      val mcf = marginalCostFunction.build()
      val cf  = (e: EdgeBPR) => mcf(e)(Flow.Zero)

      new kSPwLO_SVP_Sync(
        k = k,
        theta = theta,
        terminationFunction = kspTerminationFunction.build(),
        costFunction = cf,
        minBatchSize = minBatchSize
      )
    }
  }
}
