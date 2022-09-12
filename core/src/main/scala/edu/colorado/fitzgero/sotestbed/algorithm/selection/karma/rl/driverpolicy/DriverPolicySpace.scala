package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.config.DriverPolicyConfig
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal.BernoulliDistributionSampling
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal.BetaDistributionSampling
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal.ThresholdSampling
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal.UserOptimal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.RandomPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.CongestionProportionalThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ScaledProportionalThreshold

sealed trait DriverPolicySpace

object DriverPolicySpace {

  /**
    * encode the agent's karma balance as a feature
    */
  case object Balance extends DriverPolicySpace

  /**
    * encode the agent's difference between their original travel time estimate
    * and their current estimate as a feature
    */
  case object Urgency extends DriverPolicySpace

  /**
    * encode the travel time difference between an agent's original travel time
    * estimate and the estimate for the worst alternative path the agent is
    * offered by the routing system
    */
  case object WorstAlternative extends DriverPolicySpace

  /**
    * encode the number of agents in this batch as a feature
    */
  case object BatchSize extends DriverPolicySpace

  /**
    * pass along the network signal so we understand better what we are fighting for
    */
  case object NetworkSignal extends DriverPolicySpace

  /**
    * remaining travel distance along the current path
    */
  case object RemainingDistance extends DriverPolicySpace

  /**
    * the travel time estimated for the currently-assigned route
    */
  case object RemainingTravelTimeEstimate extends DriverPolicySpace

  /**
    * count of replanning events already experienced
    */
  case object ReplanningEvents extends DriverPolicySpace

  /**
    * use some combination of features
    *
    * @param features the features to encode
    */
  case class Combined(features: List[DriverPolicySpace]) extends DriverPolicySpace

  implicit class DPSExtensions(dps: DriverPolicySpace) {

    /**
      * encodes an observation based on the current network state for
      * some request along with information based on its history and
      * the proposed set of routes for this agent.
      *
      * @param rn the current road network state
      * @param cf a cost function
      * @param request the current agent request
      * @param balance the current karma balance for this agent
      * @param history the route plan history for this agent
      * @param proposedPaths the set of paths that the agent could be assigned a new route from
      * @return the effect of collecting a list of observation values
      */
    def encodeObservation(
      request: Request,
      balance: Karma,
      history: AgentHistory,
      proposedPaths: List[Path],
      batch: Map[Request, List[Path]],
      networkSignal: NetworkPolicySignal
    ): IO[List[Double]] = {
      dps match {
        case Balance =>
          IO.pure(List(balance.value))
        case Urgency =>
          ObservationOps.travelTimeDiffFromInitialTrip(history).map { u => List(u) }
        case WorstAlternative =>
          ObservationOps
            .travelTimeDiffFromAlternatives(history, proposedPaths)
            .map { v => List(v.max) }
        case BatchSize =>
          IO.pure(List(batch.size))
        case NetworkSignal =>
          networkSignal match {
            case BernoulliDistributionSampling(thresholdPercent, bernoulliPercent) =>
              IO.pure(List(thresholdPercent, bernoulliPercent))
            case BetaDistributionSampling(dist) =>
              IO.pure(List(dist.getAlpha, dist.getBeta))
            case ThresholdSampling(thresholdPercent, random) =>
              IO.pure(List(thresholdPercent))
            case UserOptimal =>
              val msg = "cannot use UserOptimal network policy with DriverPolicySpace.NetworkPolicy"
              IO.raiseError(new Error(msg))
          }
        case RemainingDistance =>
          val dist = history.currentRequest.remainingRoute.foldLeft(0.0) { _ + _.linkDistance }
          IO.pure(List(dist))
        case RemainingTravelTimeEstimate =>
          val ttEstimate = history.currentRequest.remainingRoute.foldLeft(0.0) {
            _ + _.estimatedTimeAtEdge.getOrElse(SimTime.Zero).value.toDouble
          }
          IO.pure(List(ttEstimate))
        case ReplanningEvents =>
          IO.pure(List(history.replanningEvents))
        case Combined(features) =>
          features.flatTraverse { _.encodeObservation(request, balance, history, proposedPaths, batch, networkSignal) }
      }
    }

    def encodeFinalObservation(
      originalTravelTimeEstimate: SimTime,
      finalTravelTime: SimTime,
      finalBankBalance: Karma,
      networkPolicyConfig: NetworkPolicyConfig
    ): IO[List[Double]] = dps match {
      case Balance =>
        IO.pure(List(finalBankBalance.value.toDouble))
      case Urgency =>
        val urgency = (finalTravelTime - originalTravelTimeEstimate).value.toDouble
        IO.pure(List(urgency))
      case WorstAlternative =>
        IO.pure(List(0.0))
      case BatchSize =>
        IO.pure(List(0.0))
      case NetworkSignal =>
        // generally here, we just want to make sure that the number of feature matches what
        // would be encoded into each observation throughout the day; it should clearly be
        // a network signal that requests zero % SO routing.
        IO.pure(List(0.0))
      case RemainingDistance =>
        IO.pure(List(0.0))
      case RemainingTravelTimeEstimate =>
        IO.pure(List(0.0))
      case ReplanningEvents =>
        IO.pure(List(0.0)) // would it be better if we stored the final count instead?
      case Combined(features) =>
        features.flatTraverse(
          _.encodeFinalObservation(
            originalTravelTimeEstimate,
            finalTravelTime,
            finalBankBalance,
            networkPolicyConfig
          )
        )
    }
  }
}
