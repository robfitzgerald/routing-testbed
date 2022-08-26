package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness._
import cats.implicits._
import cats.effect._
import java.nio.file.{Path => JavaNioPath}
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import com.typesafe.scalalogging.LazyLogging
import java.io.PrintWriter
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import scala.util.Failure
import scala.util.Success

object KarmaSelectionRlOps extends LazyLogging {

  // def startEpisode(
  //   structure: RLDriverPolicyStructure,
  //   client: RLDriverPolicyClient,
  //   req: Request
  // ): IO[PolicyClientResponse] = {
  //   structure match {
  //     case RLDriverPolicyStructure.MultiAgentPolicy(space) =>
  //       throw new NotImplementedError
  //     case RLDriverPolicyStructure.SingleAgentPolicy(space) =>
  //       // start episodes for any new agents
  //       val agentId = req.agent
  //       logger.info(s"starting RL episode for agent ${req.agent}")
  //       val startReq = PolicyClientRequest.StartEpisodeRequest(
  //         Some(EpisodeId(agentId)),
  //         client.trainingEnabled
  //       )
  //       client.send(startReq)
  //   }
  // }

  def endEpisodes(
    structure: RLDriverPolicyStructure,
    client: RLDriverPolicyClient,
    networkPolicyConfig: NetworkPolicyConfig,
    experimentDirectory: JavaNioPath,
    allocationTransform: AllocationTransform,
    agentsWithEpisodes: Set[String],
    finalBank: Map[String, Karma],
    episodePrefix: String
  ) = {
    structure match {
      case RLDriverPolicyStructure.MultiAgentPolicy(space) =>
        IO.raiseError(new NotImplementedError)
      case policy: RLDriverPolicyStructure.SingleAgentPolicy =>
        import PolicyClientRequest._

        // trip log contains the initial travel time estimate along with the final travel time, but not all
        // agents in the trip log can be guaranteed to have RL episodes
        // see [[]]
        // here, we filter the trip log to the agents with known RL episodes before sending requests to log + close
        // those episodes. we also log all of the final rewards and observations to a file.
        val result: IO[Unit] = for {
          tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
          tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
          // tripsSorted       = tripsWithEpisodes.sortBy { _.agentId }(agentIdOrdering)
          rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByTripComparison(tripsWithEpisodes, allocationTransform)
          observations <- RLDriverPolicyEpisodeOps.finalObservations(
            tripsWithEpisodes,
            policy.space,
            networkPolicyConfig,
            finalBank
          )
          logRewardsMsgs = rewards.map { case (a, r)      => LogReturnsRequest(EpisodeId(a, episodePrefix), r) }
          endEpisodeMsgs = observations.map { case (a, o) => EndEpisodeRequest(EpisodeId(a, episodePrefix), o) }
          _ <- client.send(logRewardsMsgs)
          _ <- client.send(endEpisodeMsgs) // needs to happen AFTER logging final reward for all agents
          _ <- logFinalRewards(rewards, observations, experimentDirectory)
        } yield logger.info(s"finished all RL episodes")

        result
    }
  }

  def logFinalRewards(
    rewards: List[(String, Reward)],
    observations: List[(String, Observation)],
    experimentDirectory: JavaNioPath
  ): IO[Unit] = {
    IO.fromTry {
      Try {
        val pw = new PrintWriter(f"$experimentDirectory/final_rewards.csv")
        pw.write(s"agentId,reward,observation" + "\n")
        val rewardsLookup = rewards.toMap
        for {
          (a, o) <- observations
          r      <- rewardsLookup.get(a)
        } {
          pw.write(f"""$a,${r.prettyPrint},"${o.prettyPrint}"""" + "\n")
        }
        pw.close()
      }
    }
  }

  // /**
  //  * process agents by trip id and then agent id. this should help process all
  //  * episodes
  //  *
  //  */
  // def agentIdOrdering: Ordering[String] = Ordering.by { s =>
  //   s.split('-').lastOption match {
  //     case None => throw new Error(s"agentIds expected to have hyphens, found $s")
  //     case Some(value) =>
  //       Try { value.toInt } match {
  //         case Failure(e) =>
  //           throw new Error(s"agentIds expected to end with a number, but found $value", e)
  //         case Success(value) =>
  //           // sort by trip ID (Integers), then agent ID (lexicagraphically)
  //           (value, s)
  //       }
  //   }
  // }
}
