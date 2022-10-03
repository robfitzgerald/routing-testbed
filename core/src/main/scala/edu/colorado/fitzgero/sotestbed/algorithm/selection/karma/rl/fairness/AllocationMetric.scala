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

sealed trait AllocationMetric

object AllocationMetric extends LazyLogging {

  case object TripComparison          extends AllocationMetric
  case object AccumulatedAuctionDelay extends AllocationMetric

  implicit class AllocationMetricExtension(am: AllocationMetric) {

    def collectFinalRewardsAndObservations(
      experimentDirectory: JavaNioPath,
      agentsWithEpisodes: Set[String],
      allocationTransform: AllocationTransform,
      driverPolicySpace: DriverPolicySpace,
      networkPolicyConfig: NetworkPolicyConfig,
      finalBank: Map[String, Karma]
    ): IO[(List[(String, Double)], List[(String, List[Double])])] = {

      am match {
        case TripComparison =>
          for {
            tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
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
            tripLogs  <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
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
      }

    }

  }

}
