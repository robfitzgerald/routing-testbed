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

object KarmaSelectionRlOps extends LazyLogging {

  def startEpisode(
    structure: RLDriverPolicyStructure,
    client: RLDriverPolicyClient,
    req: Request
  ): IO[PolicyClientResponse] = {
    structure match {
      case RLDriverPolicyStructure.MultiAgentPolicy(space) =>
        throw new NotImplementedError
      case RLDriverPolicyStructure.SingleAgentPolicy(space) =>
        // start episodes for any new agents
        val agentId = req.agent
        logger.info(s"starting RL episode for agent ${req.agent}")
        val startReq = PolicyClientRequest.StartEpisodeRequest(
          Some(EpisodeId(agentId)),
          client.trainingEnabled
        )
        client.send(startReq)
    }
  }

  def endEpisodes(
    structure: RLDriverPolicyStructure,
    client: RLDriverPolicyClient,
    experimentDirectory: JavaNioPath,
    allocationTransform: AllocationTransform,
    finalBank: Map[String, Karma]
  ) = {
    structure match {
      case RLDriverPolicyStructure.MultiAgentPolicy(space) =>
        IO.raiseError(new NotImplementedError)
      case policy: RLDriverPolicyStructure.SingleAgentPolicy =>
        // function to map an agent's id, final reward, and final observation to
        // two server requests. the first request logs the final reward, and the
        // second request ends the training episode.
        val toClientMessageFn: ((String, Reward), Observation) => List[PolicyClientRequest] =
          (agentIdAndReward, observation) => {
            val (agentId, reward) = agentIdAndReward
            logger.info(s"ending RL episode for agent $agentId")
            val finalRewardReq = PolicyClientRequest.LogReturnsRequest(
              episode_id = EpisodeId(agentId),
              reward = reward,
              info = Map.empty,
              done = Some(Map(AgentId(agentId) -> true))
            )
            val endEpisodeReq = PolicyClientRequest.EndEpisodeRequest(
              episode_id = EpisodeId(agentId),
              observation = observation
            )
            val reqs: List[PolicyClientRequest] = List(finalRewardReq, endEpisodeReq)
            reqs
          }

        // for all requests, send a final reward signal and close the agent's training episode
        val result: IO[Unit] = for {
          tripLogs     <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
          rewards      <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByTripComparison(tripLogs, allocationTransform)
          observations <- RLDriverPolicyEpisodeOps.finalObservations(tripLogs, policy.space, finalBank)
          requests = rewards.zip(observations).flatMap(toClientMessageFn.tupled)
          batches  = requests.sliding(client.parallelism, client.parallelism).toList
          _ <- batches.traverse { _.parTraverse { req => client.send(req) } }
        } yield ()

        result
    }
  }
}
