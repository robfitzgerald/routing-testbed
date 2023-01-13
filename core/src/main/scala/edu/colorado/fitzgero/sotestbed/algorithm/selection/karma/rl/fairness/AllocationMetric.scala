package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness._
import cats.implicits._
import cats.effect._
import java.nio.file.{Path => JavaNioPath}
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Bid
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpaceV2Ops

/**
  * driver policies use an AllocationMetric to capture the allocations of various drivers
  */
sealed trait AllocationMetric

object AllocationMetric extends LazyLogging {

  /**
    * use difference between a separate "selfish-only" trial and the
    * current one to compute diffs. uses the AllocationTransform argument
    * to reshape the distribution before computing the fairness.
    */
  case object SelfishTripDiff extends AllocationMetric

  /**
    * observes the marginal change in delay between each auction for each
    * agent in order to more closely track what the agent believes they
    * are choosing between when computing the reward.
    */
  case object AccumulatedAuctionDelay extends AllocationMetric

  /**
    * observes the final difference between experienced and free flow
    * travel speed for a route. this is transformed into a percentage range
    * where 100% means the agent experienced no delay from free flow, and
    * 0% means the agent experienced an infinite delay, by using the travel
    * distance to transpose travel times to speeds, and then dividing the
    * free flow speed by the experienced speed.
    */
  case object FreeFlowDiffProportion extends AllocationMetric

  implicit class AllocationMetricExtension(am: AllocationMetric) {

    /**
      * computes an allocation metric at the time of decision. when a
      * path alternative has been selected, we can estimate the allocation
      * for certain allocation metrics (not all) and use it in a batch-wise
      * fairness calculation.
      *
      * @param request the request to get the allocation for
      * @param selectedPathSpur (ksp) path selected for this agent
      * @param aah history of agent requests
      * @param rn road network state
      * @return effect of computing an allocation
      */
    def batchWiseAllocation(
      request: Request,
      selectedPathSpur: Path,
      aah: ActiveAgentHistory,
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
    ): IO[Double] = am match {
      case SelfishTripDiff         => IO.raiseError(new NotImplementedError)
      case AccumulatedAuctionDelay => IO.raiseError(new NotImplementedError)
      case FreeFlowDiffProportion =>
        for {
          hist <- IO.fromEither(aah.getAgentHistoryOrError(request.agent))
          // tt <- DriverPolicySpaceV2Ops.pathAlternativeTravelTimeEstimate(hist, path)
          current    <- DriverPolicySpaceV2Ops.currentRoute(hist)
          currentReq <- IO.fromEither(hist.currentRequest)
          spurEdges  <- selectedPathSpur.traverse(_.toEdgeData(rn))
          remWithSpur = DriverPolicySpaceV2Ops.coalesceFuturePath(currentReq.remainingRoute, spurEdges)
          futurePath  = DriverPolicySpaceV2Ops.coalesceFuturePath(current, spurEdges)
          dists       = futurePath.map(_.linkDistance)
          tts <- DriverPolicySpaceV2Ops.travelTime(rn, currentReq.experiencedRoute, remWithSpur)
          ffs <- DriverPolicySpaceV2Ops.freeFlowTravelTime(rn, futurePath)
          dist = dists.foldLeft(0.0) { _ + _ }
          tt   = tts.foldLeft(0.0) { _ + _ }
          ff   = ffs.foldLeft(0.0) { _ + _ }
        } yield RewardOps.freeFlowSpeedDiffAllocation(tt, ff, dist)
    }

    def collectFinalRewardsAndObservations(
      experimentDirectory: JavaNioPath,
      agentsWithEpisodes: Set[String],
      allocationTransform: AllocationTransform,
      driverPolicySpace: DriverPolicySpaceV2,
      networkPolicyConfig: NetworkPolicyConfig,
      roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      finalBank: Map[String, Karma]
    ): IO[(List[(String, Double)], List[(String, List[Double])])] = {

      am match {
        case SelfishTripDiff =>
          for {
            tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory, includeUoAgents = false)
            tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
            rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByTripComparison(
              tripsWithEpisodes,
              allocationTransform
            )
            observations <- RLDriverPolicyEpisodeOps.finalObservations(
              tripsWithEpisodes,
              driverPolicySpace,
              networkPolicyConfig,
              finalBank
            )
          } yield (rewards, observations)
        case AccumulatedAuctionDelay =>
          for {
            tripLogs  <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory, includeUoAgents = false)
            karmaLogs <- RLDriverPolicyEpisodeOps.getKarmaLog(experimentDirectory)
            tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
            karmaWithEpisodes = karmaLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
            rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByAuctionDelay(karmaWithEpisodes, tripsWithEpisodes)
            observations <- RLDriverPolicyEpisodeOps.finalObservations(
              tripsWithEpisodes,
              driverPolicySpace,
              networkPolicyConfig,
              finalBank
            )
          } yield (rewards, observations)
        case FreeFlowDiffProportion =>
          for {
            tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory, includeUoAgents = false)
            tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
            rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByFreeFlowDiff(tripsWithEpisodes)
            observations <- RLDriverPolicyEpisodeOps.finalObservations(
              tripsWithEpisodes,
              driverPolicySpace,
              networkPolicyConfig,
              finalBank
            )
          } yield (rewards, observations)
      }

    }

  }

}
