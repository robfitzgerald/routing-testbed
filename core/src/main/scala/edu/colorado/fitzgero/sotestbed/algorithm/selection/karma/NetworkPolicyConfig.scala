package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Random

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerRequest
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait NetworkPolicyConfig

object NetworkPolicyConfig {

  case object UserOptimal extends NetworkPolicyConfig

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
  case class BernoulliProportional(seed: Option[Long]) extends NetworkPolicyConfig

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
  case class BernoulliScaled(scale: Double, seed: Option[Long]) extends NetworkPolicyConfig

  implicit class NetworkPolicyExtensionMethods(policy: NetworkPolicyConfig) {

    def buildGenerator: NetworkPolicySignalGenerator = policy match {
      case UserOptimal => NetworkPolicySignalGenerator.UserOptimalGenerator
      case BernoulliProportional(seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.ProportionalBernoulliGenerator(rng)
      case BernoulliScaled(scale, seed) =>
        val rng = new Random(seed.getOrElse(System.currentTimeMillis))
        NetworkPolicySignalGenerator.ScaledBernoulliGenerator(scale, rng)
    }

    def logHeader: String = policy match {
      case UserOptimal              => ""
      case _: BernoulliProportional => ""
      case _: BernoulliScaled       => "scale"
    }

    def getLogData: String = policy match {
      case UserOptimal               => ""
      case BernoulliProportional(_)  => ""
      case BernoulliScaled(scale, _) => scale.toString
    }
  }
}