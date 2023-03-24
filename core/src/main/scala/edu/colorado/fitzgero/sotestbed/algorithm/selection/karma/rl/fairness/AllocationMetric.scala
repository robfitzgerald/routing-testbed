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

  /**
    * considers the protected resource to be "replannings" aka path updates.
    * reports the average distance per replanning event as replannings per trip distance (meters).
    * if replannings is zero, reports an allocation of zero.
    *
    * to avoid the asymptote, and to reflect the existence of the original trip route
    * assignment, we always add 1 to the number of replannings.
    */
  case object ReplanningsPerUnitDistance extends AllocationMetric

  implicit class AllocationMetricExtension(am: AllocationMetric) {

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

        case ReplanningsPerUnitDistance =>
          for {
            tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory, includeUoAgents = false)
            tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
            rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByReplanningsPerUnitDistance(tripsWithEpisodes)
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
