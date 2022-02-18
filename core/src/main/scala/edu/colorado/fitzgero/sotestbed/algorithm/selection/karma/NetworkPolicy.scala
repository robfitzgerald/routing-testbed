package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.util.Random

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerRequest
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait NetworkPolicy

object NetworkPolicy {

  case object UserOptimalPolicy extends NetworkPolicy

  /**
    * interprets the weighted congestion for a batch as a p value for the
    * network policy signal, which is used to sample from in a weighted coin flip:
    * p = bias * congestion
    * @param scale a bias parameter which can scale the congestion value before it
    *              is used as a Bernoulli parameter. for example, if a congestion of
    *              0.01 is meaningful, it could be scaled by a factor of 10 so that
    *              the Bernoulli p value is 10%, not 1%.
    */
  case class BernoulliSamplePolicy(scale: Double) extends NetworkPolicy

  implicit class NetworkPolicyExtensionMethods(policy: NetworkPolicy) {

    def header: String = policy match {
      case UserOptimalPolicy        => ""
      case _: BernoulliSamplePolicy => "scale"
    }

    def signalHeader: String = policy match {
      case UserOptimalPolicy        => ","
      case _: BernoulliSamplePolicy => "percent,"
//      case _: BernoulliDistSampleWithSODegree => "percent,degree"
//      case _: BetaDistributionSampling => "alpha,beta"
    }

    def getCoefString: String = policy match {
      case UserOptimalPolicy            => ""
      case BernoulliSamplePolicy(scale) => scale.toString
    }

    /**
      * generates the [[NetworkPolicySignal]]s used by Karma-based selection algorithms
      * @param roadNetwork the current network state
      * @param marginalCostFunction function for computing the cost
      * @param subBatches the sub-batches of this batch
      * @param congestionObservation the function for observing congestion for a sub-batch
      * @return the effect of building the set of network policy signals by sub-batch id
      */
    def generatePolicySignal(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      marginalCostFunction: EdgeBPR => Flow => Cost,
      subBatches: List[SelectionRunnerRequest],
      congestionObservation: CongestionObservation,
      random: Random
    ): IO[Map[String, NetworkPolicySignal]] = {

      policy match {

        case UserOptimalPolicy =>
          val result = subBatches.map {
            case SelectionRunnerRequest(label, _) => (label, NetworkPolicySignal.UserOptimal)
          }.toMap
          IO.pure(result)

        case p: BernoulliSamplePolicy =>
          subBatches
            .traverse {
              case SelectionRunnerRequest(batchId, batch) =>
                val edges = batch.keys.toList.map { _.location }.distinct
                congestionObservation
                  .observeCongestion(roadNetwork, marginalCostFunction, edges)
                  .map { congestion =>
                    val pct = congestion * p.scale
                    (batchId, NetworkPolicySignal.BernoulliDistributionSampling(pct, random, congestion))
                  }
            }
            .map { _.toMap }
      }
    }
  }
}
