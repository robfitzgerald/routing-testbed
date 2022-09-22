package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Random

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerRequest
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.RayRLlibClient
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkPolicySpace

sealed trait NetworkPolicyConfig

object NetworkPolicyConfig {

  case object UserOptimal extends NetworkPolicyConfig

  /**
    * selects a random policy, useful for testing
    *
    * @param seed starting seed value for random generation
    */
  case class RandomPolicy(space: NetworkPolicySpace, seed: Option[Long]) extends NetworkPolicyConfig

  /**
    * use a weighted sampling without replacement method for assignment
    *
    * @param space environment for the controller to control
    * @param seed optional starting seed value
    */
  case class CongestionWeightedSampling(space: NetworkPolicySpace, seed: Option[Long]) extends NetworkPolicyConfig

  /**
    * generates a network signal that is a Bernoulli distribution, where
    * p is the proportional increase in travel times due to current
    * congestion effects, as measured by the chosen cost/flow function
    * and congestion observation type.
    *
    * for example, if free flow speed was found to be 10.0, and observed
    * travel time was 15.0, the p value would be set to (15-10)/10 = 0.5.
    *
    * p is restricted to the range [0.0, 1.0].
    *
    * @param seed random number generator seed value
    */
  case class CongestionThreshold(space: NetworkPolicySpace, seed: Option[Long]) extends NetworkPolicyConfig

  /**
    * generates a network signal that is a Bernoulli distribution, where
    * p is the proportional increase in travel times due to current
    * congestion effects, as measured by the chosen cost/flow function
    * and congestion observation type.
    *
    * the scale value is multiplied against the proportional increase of observed
    * travel time to free flow travel time. for example, if free flow is 10,
    * observed is 30, and scale=0.01, then p is ((30-10)/10) * 0.1 = 0.2.
    * may be useful if meaningful increase values for a cost/flow function
    * map into domains that are 1) known, 2) vastly [larger] than the domain [0,1].
    *
    * the scaled p value is restricted to the range [0.0, 1.0].
    *
    * @param scale a bias parameter which can scale the travel time increase value
    *              before it is used as the Bernoulli "p" parameter.
    * @param seed random number generator seed value
    */
  case class ScaledProportionalThreshold(space: NetworkPolicySpace, scale: Double, seed: Option[Long])
      extends NetworkPolicyConfig

  case class ExternalRLServer(underlying: NetworkPolicyConfig, space: NetworkPolicySpace, client: RayRLlibClient)
      extends NetworkPolicyConfig

  implicit class NetworkPolicyExtensionMethods(policy: NetworkPolicyConfig) {

    def space: Option[NetworkPolicySpace] = policy match {
      case UserOptimal                                     => None
      case RandomPolicy(space, seed)                       => Some(space)
      case CongestionWeightedSampling(space, seed)         => Some(space)
      case CongestionThreshold(space, seed)                => Some(space)
      case ScaledProportionalThreshold(space, scale, seed) => Some(space)
      case ExternalRLServer(underlying, space, client)     => Some(space)
    }

    def buildGenerator: NetworkPolicySignalGenerator = policy match {
      case UserOptimal => NetworkPolicySignalGenerator.UserOptimalGenerator
      case RandomPolicy(space, seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.RandomGenerator(rng)
      case CongestionWeightedSampling(space, seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.WeightedSamplingGenerator(rng)
      case CongestionThreshold(space, seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.ThresholdSamplingGenerator(rng)
      case ScaledProportionalThreshold(space, scale, seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.ScaledThresholdSamplingGenerator(scale, rng)
      case ext: ExternalRLServer => ext.underlying.buildGenerator
    }

    def logHeader: String = policy match {
      case UserOptimal                    => ""
      case _: RandomPolicy                => ""
      case _: CongestionWeightedSampling  => ""
      case _: CongestionThreshold         => ""
      case _: ScaledProportionalThreshold => "scale"
      case ext: ExternalRLServer          => ext.underlying.logHeader
    }

    def getLogData: String = policy match {
      case UserOptimal                              => ""
      case _: RandomPolicy                          => ""
      case _: CongestionWeightedSampling            => ""
      case CongestionThreshold(_, _)                => ""
      case ScaledProportionalThreshold(_, scale, _) => scale.toString
      case ext: ExternalRLServer                    => ext.underlying.getLogData
    }
  }
}
