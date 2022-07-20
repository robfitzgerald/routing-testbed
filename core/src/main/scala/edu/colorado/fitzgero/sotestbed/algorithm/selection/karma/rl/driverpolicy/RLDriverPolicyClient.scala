package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import java.io.InputStream

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.RLDriverPolicyStructure
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma._
import edu.colorado.fitzgero.sotestbed.rllib.Action

final class RLDriverPolicyClient(host: String, port: Int, episodeId: EpisodeId) {

  /**
    * sends a request to end an episode along with the final observation
    *
    * @param obs the final observation
    * @param reward the episode rewards if any
    * @param info the final info, if any
    * @param done done messages per agent
    * @return the effect of sending the LogReturnsRequest
    */
  def finishEpisode(
    obs: Observation,
    reward: Reward,
    info: Map[String, String],
    done: Option[Map[AgentId, Boolean]] = None
  ): IO[Unit] = {
    val req = LogReturnsRequest(episodeId, reward, info, done)
    for {
      _ <- PolicyClientOps.send(req, host, port)
    } yield ()
  }

  /**
    * function that handles communication to the RL server. knowledge of the
    * host, port, and EpisodeId are persistent between calls, but, the state
    * of the simulation varies.
    *
    * @param req a request
    * @param obs the observation associated with this request
    * @return the effect of collecting an action for this agent
    */
  def getBidAction(
    req: Request,
    obs: Observation
  ): IO[Action] = {
    PolicyClientOps
      .send(GetActionRequest(episodeId, obs), host, port)
      .flatMap { response =>
        response match {
          case GetActionResponse(action) =>
            IO.pure(action)
          case other =>
            val msg = s"GetActionRequest response was not GetActionResponse, found $other"
            IO.raiseError(new Error(msg))
        }
      }
  }
}

object RLDriverPolicyClient {

  def createClientAndEpisode(host: String, port: Int): IO[RLDriverPolicyClient] = {
    for {
      res       <- PolicyClientOps.send(PolicyClientRequest.StartEpisodeRequest(), host, port)
      episodeId <- res.getEpisodeId
    } yield new RLDriverPolicyClient(host, port, episodeId)
  }
}
