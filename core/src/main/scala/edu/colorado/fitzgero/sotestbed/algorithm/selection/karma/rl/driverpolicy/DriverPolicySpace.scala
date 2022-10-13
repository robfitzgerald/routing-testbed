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
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.RandomPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.CongestionThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ScaledProportionalThreshold
import edu.colorado.fitzgero.sotestbed.model.numeric.Meters
import java.sql.Driver

sealed trait DriverPolicySpace

object DriverPolicySpace {

  /**
    * encode the agent's karma balance as a feature
    */
  case object Balance extends DriverPolicySpace

  /**
    * encode the number of agents in this batch as a feature
    */
  case object BatchSize extends DriverPolicySpace

  /**
    * count of replanning events already experienced
    */
  case object ReplanningEvents extends DriverPolicySpace

  /**
    * percent of auctions this agent has won, default 0% if no auctions have been experienced
    */
  case object AuctionWinRate extends DriverPolicySpace

  /**
    * while observing other values, keep in perspective the original trip distance
    */
  case object OriginalDistance extends DriverPolicySpace

  /**
    * remaining travel distance along the current path
    */
  case object RemainingDistance extends DriverPolicySpace

  /**
    * experienced distance for the currently-assigned route
    */
  case object ExperiencedDistance extends DriverPolicySpace

  /**
    * at a replanning event, captures the marginal change in
    * distance due to selecting the UO route (aka winning)
    */
  case object MarginalUODistance extends DriverPolicySpace

  /**
    * at a replanning event, captures the marginal change in
    * distance due to selecting the worst SO route (aka losing)
    */
  case object MarginalWorstSODistance extends DriverPolicySpace

  /**
    * while observing other values, keep in perspective the original travel time estimate
    */
  case object OriginalTravelTimeEstimate extends DriverPolicySpace

  /**
    * experienced travel time for the currently-assigned route
    */
  case object ExperiencedTravelTime extends DriverPolicySpace

  /**
    * the travel time estimated for the currently-assigned route
    */
  case object RemainingTravelTimeEstimate extends DriverPolicySpace

  /**
    * at a replanning event, captures the marginal change in
    * distance due to selecting the UO route (aka winning)
    */
  case object MarginalUOTravelTime extends DriverPolicySpace

  /**
    * at a replanning event, captures the marginal change in
    * distance due to selecting the worst SO route (aka losing)
    */
  case object MarginalWorstSOTravelTime extends DriverPolicySpace

  /**
    * at a replanning event, captures the diff from free flow
    * for the
    */
  case object FreeFlowDiffExperiencedTravelTime extends DriverPolicySpace

  /**
    * at a replanning event, captures the diff from free flow
    * travel time due to selecting the user-optimal route (aka winning)
    */
  case object FreeFlowDiffUOTravelTime extends DriverPolicySpace

  /**
    * at a replanning event, captures the diff from free flow
    * travel time due to selecting the worst SO route (aka losing)
    */
  case object FreeFlowDiffWorstSOTravelTime extends DriverPolicySpace

  /**
    * encode the travel time difference between an agent's original travel time
    * estimate and the estimate for the worst alternative path the agent is
    * offered by the routing system
    */
  case object WorstAlternative extends DriverPolicySpace

  /**
    * encode the agent's difference between their original travel time estimate
    * and their current estimate as a feature
    */
  case object OriginalTravelTimeDiff extends DriverPolicySpace

  /**
    * offset as ((so-ff) - (uo-ff)) / (uo-ff)
    */
  case object MarginalFreeFlowUOToSOOffset extends DriverPolicySpace

  /**
    * pass along the network signal so we understand better what we are fighting for
    */
  case object NetworkSignal extends DriverPolicySpace

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
      roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      history: AgentHistory,
      proposedPaths: List[Path],
      batch: Map[Request, List[Path]],
      networkSignal: NetworkPolicySignal
    ): IO[List[Double]] = {
      dps match {
        ////////////////////////////////// AGENT STATE FEATURES
        case Balance =>
          IO.pure(List(balance.value))
        case BatchSize =>
          IO.pure(List(batch.size))
        case ReplanningEvents =>
          IO.pure(List(history.replanningEvents))
        case AuctionWinRate =>
          IO.pure(List(history.uoAssignmentRate))
        ////////////////////////////////// DISTANCE FEATURES
        case OriginalDistance =>
          val dist = history.first.tripDistance.value
          IO.pure(List(dist))
        case ExperiencedDistance =>
          for {
            current <- IO.fromEither(history.currentRequest)
          } yield List(current.experiencedDistance.value)
        case RemainingDistance =>
          for {
            current <- IO.fromEither(history.currentRequest)
          } yield List(current.remainingDistance.value)
        case MarginalUODistance =>
          for {
            currentRoute <- IO.fromEither(history.currentRequest.map { _.remainingRoute })
            currentDist = currentRoute.foldLeft(0.0) { _ + _.linkDistance }
            coalesceFn  = ObservationOps.distanceCostOfRootAndSpur(roadNetwork, currentRoute)
            uoRoute     <- IO.fromOption(proposedPaths.headOption)(new Error(s"request $request missing alt paths"))
            uoRouteDist <- coalesceFn(uoRoute)
          } yield List(currentDist - uoRouteDist)
        case MarginalWorstSODistance =>
          for {
            currentRoute <- IO.fromEither(history.currentRequest.map { _.remainingRoute })
            currentDist = currentRoute.foldLeft(0.0) { _ + _.linkDistance }
            coalesceFn  = ObservationOps.distanceCostOfRootAndSpur(roadNetwork, currentRoute)
            maxSoRouteDist <- proposedPaths.traverse { coalesceFn }.map { _.max }
          } yield List(currentDist - maxSoRouteDist)
        ////////////////////////////////// TIME FEATURES
        case OriginalTravelTimeEstimate =>
          for {
            ttEst <- IO.fromEither(history.first.tripTravelTimeEstimate)
          } yield List(ttEst.value.toDouble)
        case ExperiencedTravelTime =>
          for {
            current <- IO.fromEither(history.currentRequest)
          } yield List(current.experiencedTravelTime.value)
        case RemainingTravelTimeEstimate =>
          for {
            current   <- IO.fromEither(history.currentRequest)
            remaining <- IO.fromEither(current.remainingTravelTimeEstimate)
          } yield List(remaining.value)
        case MarginalUOTravelTime =>
          for {
            currentRoute <- IO.fromEither(history.currentRequest.map { _.remainingRoute })
            currentTimeEsts <- currentRoute.traverse { e =>
              IO.fromOption(e.estimatedTimeAtEdge)(new Error(s"edge ${e.edgeId} missing time estimate"))
            }
            currentTime = currentTimeEsts.foldLeft(0.0) { _ + _.value }
            coalesceFn  = ObservationOps.timeCostOfRootAndSpur(roadNetwork, currentRoute)
            uoRoute     <- IO.fromOption(proposedPaths.headOption)(new Error(s"request $request missing alt paths"))
            uoRouteTime <- coalesceFn(uoRoute)
          } yield List(currentTime - uoRouteTime)
        case MarginalWorstSOTravelTime =>
          for {
            currentRoute <- IO.fromEither(history.currentRequest.map { _.remainingRoute })
            currentTimeEsts <- currentRoute.traverse { e =>
              IO.fromOption(e.estimatedTimeAtEdge)(new Error(s"edge ${e.edgeId} missing time estimate"))
            }
            currentTime = currentTimeEsts.foldLeft(0.0) { _ + _.value }
            coalesceFn  = ObservationOps.timeCostOfRootAndSpur(roadNetwork, currentRoute)
            maxSoRouteTime <- proposedPaths.traverse { coalesceFn }.map { _.max }
          } yield List(currentTime - maxSoRouteTime)
        case FreeFlowDiffExperiencedTravelTime =>
          for {
            req <- IO.fromEither(history.currentRequest)
            experiencedRoute = req.experiencedRoute.map { _.toPathSegment }
            experiencedDiffTravelTime <- ObservationOps.compareRouteToFreeFlow(roadNetwork, req.experiencedRoute)
          } yield List(experiencedDiffTravelTime)
        case FreeFlowDiffUOTravelTime =>
          for {
            req     <- IO.fromEither(history.currentRequest)
            uoRoute <- IO.fromOption(proposedPaths.headOption)(new Error(s"request $request missing alt paths"))
            diffFn = ObservationOps.compareRouteToFreeFlow(roadNetwork, req.experiencedRoute, req.remainingRoute) _
            freeFlowDiffUoTravelTime <- diffFn(uoRoute)
          } yield List(freeFlowDiffUoTravelTime)
        case FreeFlowDiffWorstSOTravelTime =>
          for {
            req <- IO.fromEither(history.currentRequest)
            diffFn = ObservationOps.compareRouteToFreeFlow(roadNetwork, req.experiencedRoute, req.remainingRoute) _
            maxFreeFlowDiffSoTravelTime <- proposedPaths.traverse { diffFn }.map { _.max }
          } yield List(maxFreeFlowDiffSoTravelTime)
        case OriginalTravelTimeDiff =>
          ObservationOps.travelTimeDiffFromInitialTrip(history).map { u => List(u) }
        case WorstAlternative =>
          for {
            remainingRoute <- IO.fromEither(history.currentRequest.map { _.remainingRoute })
            coalesceFn = ObservationOps.coalesceCostFor(remainingRoute) _
            maxCost    = proposedPaths.map { coalesceFn }.max
          } yield List(maxCost)
        case MarginalFreeFlowUOToSOOffset =>
          for {
            soList <- FreeFlowDiffWorstSOTravelTime.encodeObservation(
              request,
              balance,
              roadNetwork,
              history,
              proposedPaths,
              batch,
              networkSignal
            )
            uoList <- FreeFlowDiffUOTravelTime.encodeObservation(
              request,
              balance,
              roadNetwork,
              history,
              proposedPaths,
              batch,
              networkSignal
            )
            so <- IO.fromOption(soList.headOption)(new Error(s"missing obs value"))
            uo <- IO.fromOption(uoList.headOption)(new Error(s"missing obs value"))
            observation = (so - uo) / uo
          } yield List(observation)
        case NetworkSignal =>
          networkSignal match {
            case NetworkPolicySignal.BernoulliDistributionSampling(thresholdPercent, bernoulliPercent) =>
              IO.pure(List(thresholdPercent, bernoulliPercent))
            case NetworkPolicySignal.BetaDistributionSampling(dist) =>
              IO.pure(List(dist.getAlpha, dist.getBeta))
            case NetworkPolicySignal.ThresholdSampling(thresholdPercent, random) =>
              IO.pure(List(thresholdPercent))
            case NetworkPolicySignal.WeightedSampleWithoutReplacement(thresholdPercent, random) =>
              IO.pure(List(thresholdPercent))
            case NetworkPolicySignal.UserOptimal =>
              val msg = "cannot use UserOptimal network policy with DriverPolicySpace.NetworkPolicy"
              IO.raiseError(new Error(msg))
          }
        case Combined(features) =>
          features.flatTraverse {
            _.encodeObservation(request, balance, roadNetwork, history, proposedPaths, batch, networkSignal)
          }
      }
    }

    def encodeFinalObservation(
      originalTravelTimeEstimate: SimTime,
      finalTravelTime: SimTime,
      freeFlowTravelTime: SimTime,
      finalDistance: Meters,
      finalBankBalance: Karma,
      finalReplannings: Int,
      finalUoRoutesAssigned: Int,
      networkPolicyConfig: NetworkPolicyConfig
    ): IO[List[Double]] = dps match {
      ////////////////////////////////// AGENT STATE FEATURES
      case Balance =>
        IO.pure(List(finalBankBalance.value.toDouble))
      case BatchSize =>
        IO.pure(List(0.0))
      case ReplanningEvents =>
        IO.pure(List(finalReplannings.toDouble))
      case AuctionWinRate =>
        val rate = if (finalReplannings > 0) finalUoRoutesAssigned.toDouble / finalReplannings.toDouble else 0.0
        IO.pure(List(rate))
      ////////////////////////////////// DISTANCE FEATURES
      case OriginalDistance =>
        // not correct, but maybe not important? HMM, it actually is
        // IO.pure(List(0.0))
        IO.raiseError(new NotImplementedError)
      case ExperiencedDistance =>
        IO.pure(List(finalDistance.value))
      case RemainingDistance =>
        IO.pure(List(0.0))
      case MarginalUODistance =>
        IO.pure(List(0.0))
      case MarginalWorstSODistance =>
        IO.pure(List(0.0))
      ////////////////////////////////// TIME FEATURES
      case OriginalTravelTimeEstimate =>
        IO.pure(List(originalTravelTimeEstimate.value.toDouble))
      case ExperiencedTravelTime =>
        IO.pure(List(finalTravelTime.value.toDouble))
      case RemainingTravelTimeEstimate =>
        IO.pure(List(0.0))
      case MarginalUOTravelTime =>
        IO.pure(List(0.0))
      case MarginalWorstSOTravelTime =>
        IO.pure(List(0.0))
      case FreeFlowDiffExperiencedTravelTime =>
        val diff = (finalTravelTime - freeFlowTravelTime).value.toDouble
        IO.pure(List(diff))
      case FreeFlowDiffUOTravelTime =>
        // drivers can get there faster than free flow, btw
        val diff = (finalTravelTime - freeFlowTravelTime).value.toDouble
        IO.pure(List(diff))
      case FreeFlowDiffWorstSOTravelTime =>
        val diff = (finalTravelTime - freeFlowTravelTime).value.toDouble
        IO.pure(List(diff))
      case OriginalTravelTimeDiff =>
        val diff = (finalTravelTime - originalTravelTimeEstimate).value.toDouble
        IO.pure(List(diff))
      case WorstAlternative =>
        IO.pure(List(0.0))
      case MarginalFreeFlowUOToSOOffset =>
        IO.pure(List(0.0))
      case NetworkSignal =>
        // generally here, we just want to make sure that the number of feature matches what
        // would be encoded into each observation throughout the day; it should clearly be
        // a network signal that requests zero % SO routing.
        IO.pure(List(0.0))

      case Combined(features) =>
        features.flatTraverse(
          _.encodeFinalObservation(
            originalTravelTimeEstimate,
            finalTravelTime,
            freeFlowTravelTime,
            finalDistance,
            finalBankBalance,
            finalReplannings,
            finalUoRoutesAssigned,
            networkPolicyConfig
          )
        )
    }
  }
}
