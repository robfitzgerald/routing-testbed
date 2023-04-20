package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.Action
import edu.colorado.fitzgero.sotestbed.rllib.Action._
import edu.colorado.fitzgero.sotestbed.rllib.Reward
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest
import edu.colorado.fitzgero.sotestbed.rllib.EpisodeId
import edu.colorado.fitzgero.sotestbed.rllib.Reward.MultiAgentReward
import edu.colorado.fitzgero.sotestbed.rllib.Reward.SingleAgentReward
import edu.colorado.fitzgero.sotestbed.rllib.AgentId
import edu.colorado.fitzgero.sotestbed.rllib.Observation
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignalGenerator
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import com.typesafe.scalalogging.LazyLogging

/**
  * at this point we aren't using this abstraction because all
  * network policy signals are multiagent, but in the future,
  * the NetworkPolicySpace would be brokered through this layer
  * that would dictate the agent structure of the problem.
  */
sealed trait NetworkPolicyStructure

object NetworkPolicyStructure extends LazyLogging {

  /**
    * a single agent policy maps to a single agent RLLib
    * configuration where obs|act|rew are not grouped by agent id
    * - observation is Tupled
    */
  case object SingleAgentPolicy extends NetworkPolicyStructure

  /**
    * a multi-agent policy maps to a multiagent RLlib configuration
    * where the obs|act|rew are dictionaries keyed by (network) agent ids
    */
  case object MultiAgentPolicy extends NetworkPolicyStructure

  /**
    * a grouped multi-agent policy maps to a grouped RLlib multiagent
    * configuration where the obs|act|rew are dictionaries keyed by
    * a grouping id, but agent ids are replaced by array indices.
    * currently designed to only support one group.
    */
  case class SingleGroupMultiAgentPolicy(groupId: String) extends NetworkPolicyStructure

  implicit class NPSExtensions(nps: NetworkPolicyStructure) {

    /**
      * create a LOG_RETURNS message to ship to the RL server.
      *
      * @param episodeId RL episode associated with this message
      * @param batchId
      * @param roadNetwork
      * @param previousBatch
      * @param space
      * @return
      */
    def generateLogReturnsRequest(
      episodeId: EpisodeId,
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      previousBatch: Map[String, List[EdgeId]],
      space: NetworkPolicySpace
    ): IO[PolicyClientRequest.LogReturnsRequest] =
      nps match {
        case SingleAgentPolicy =>
          // combine the rewards for all zones/agents into a single agent reward
          for {
            reward <- space.encodeReward(roadNetwork, previousBatch)
            mar    <- reward.asMultiAgentReward
            r = mar.reward.values.foldLeft(0.0) { _ + _ }
            // flattened = mar.reward.toList.sortBy { case (agentId, _) => agentId }.map { case (_, r) => r }
            sar = Reward.SingleAgentReward(r)
          } yield PolicyClientRequest.LogReturnsRequest(episodeId, sar)
        case MultiAgentPolicy =>
          space.encodeReward(roadNetwork, previousBatch).map { rew =>
            PolicyClientRequest.LogReturnsRequest(episodeId, rew)
          }
        case SingleGroupMultiAgentPolicy(groupId) =>
          MultiAgentPolicy
            .generateLogReturnsRequest(episodeId, roadNetwork, previousBatch, space)
            .flatMap { _.reward.asMultiAgentReward }
            .map { reward =>
              val grouped     = group(groupId, reward.reward)
              val groupReward = Reward.GroupedMultiAgentReward(grouped)
              PolicyClientRequest.LogReturnsRequest(episodeId, groupReward)
            }
      }

    /**
      * observes the network state and wraps it in a request to the RL server
      * for actions.
      *
      * @param episodeId the episode that is running
      * @param roadNetwork the current road network state
      * @param zoneLookup maps names of network agents to links in the network
      * @param space the way we are encoding this environment
      *
      */
    def generateGetActionRequest(
      episodeId: EpisodeId,
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      zoneLookup: Map[String, List[EdgeId]],
      space: NetworkPolicySpace
    ): IO[PolicyClientRequest.GetActionRequest] =
      nps match {
        case SingleAgentPolicy =>
          NetworkPolicyStructureOps
            .encodeTupledObservation(episodeId, roadNetwork, zoneLookup, space)
            .map { obs => PolicyClientRequest.GetActionRequest(episodeId, obs) }

        case MultiAgentPolicy =>
          space.encodeObservation(roadNetwork, zoneLookup).map { obs =>
            PolicyClientRequest.GetActionRequest(episodeId, obs)
          }

        case SingleGroupMultiAgentPolicy(groupId) =>
          MultiAgentPolicy
            .generateGetActionRequest(episodeId, roadNetwork, zoneLookup, space)
            .flatMap { _.observation.asMultiAgentObservation }
            .map { observation =>
              val grouped            = group(groupId, observation.observation)
              val groupedObservation = Observation.GroupedMultiAgentObservation(grouped)
              PolicyClientRequest.GetActionRequest(episodeId, groupedObservation)
            }
      }

    /**
      * observes the network state and wraps it in a request to the RL server
      * to end the episode.
      *
      * @param episodeId the episode that is running
      * @param roadNetwork the current road network state
      * @param zoneLookup maps names of network agents to links in the network
      * @param space the way we are encoding this environment
      *
      */
    def generateEndEpisodeRequest(
      episodeId: EpisodeId,
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      zoneLookup: Map[String, List[EdgeId]],
      space: NetworkPolicySpace
    ): IO[PolicyClientRequest.EndEpisodeRequest] =
      nps match {
        case SingleAgentPolicy =>
          NetworkPolicyStructureOps
            .encodeTupledObservation(episodeId, roadNetwork, zoneLookup, space)
            .map { obs => PolicyClientRequest.EndEpisodeRequest(episodeId, obs) }

        case MultiAgentPolicy =>
          space.encodeObservation(roadNetwork, zoneLookup).map { obs =>
            PolicyClientRequest.EndEpisodeRequest(episodeId, obs)
          }

        case SingleGroupMultiAgentPolicy(groupId) =>
          MultiAgentPolicy
            .generateGetActionRequest(episodeId, roadNetwork, zoneLookup, space)
            .flatMap { _.observation.asMultiAgentObservation }
            .map { observation =>
              val grouped            = group(groupId, observation.observation)
              val groupedObservation = Observation.GroupedMultiAgentObservation(grouped)
              PolicyClientRequest.EndEpisodeRequest(episodeId, groupedObservation)
            }
      }

    /**
      * extracts the incoming action as network signals, making sure to unpack from the
      * provided RLlib action data object
      *
      * note: currently restricted to real-numbered action spaces only
      *
      * @param action action to extract
      * @param space policy space used
      * @param gen signal generator used
      * @param agents list of agents used by grouped policies
      * @return effect of creating signals by network AgentId
      */
    def extractActions(
      action: Action,
      space: NetworkPolicySpace,
      gen: NetworkPolicySignalGenerator,
      agents: List[String]
    ): IO[Map[String, NetworkPolicySignal]] = nps match {
      case SingleAgentPolicy =>
        action match {
          case a: Action.TupledAgentRealAction =>
            if (a.action.lengthCompare(agents) != 0) {
              val nActions = a.action.length
              val nAgents  = agents.length
              IO.raiseError(new Error(s"$nActions actions from server but $nAgents agents, must be equal"))
            } else {
              val agentIds = agents.map { s => AgentId(s) }
              val mara     = Action.MultiAgentRealAction(agentIds.sorted.zip(a.action).toMap)
              gen.generateSignalsForZones(mara)
            }
          case other => IO.raiseError(new Error(s"expected Tupled action, found ${other.getClass.getSimpleName}"))
        }
      case MultiAgentPolicy => space.decodeAction(action, gen)
      case SingleGroupMultiAgentPolicy(groupId) =>
        for {
          maAct <- Action.extractGroupedMultiAgentRealAction(action)
          agentIds = agents.map(AgentId.apply)
          ungrouped <- IO.fromEither(ungroup(maAct, groupId, agentIds))
          act = Action.MultiAgentRealAction(ungrouped)
          sigs <- MultiAgentPolicy.extractActions(act, space, gen, List.empty)
        } yield sigs
    }
  }

  /**
    * sorts the data by key and then flattens the data into a List[T]. the new
    * agent id is the batchId.
    *
    * @param batchId group id
    * @param data data to "group" for a grouped RL environment
    */
  def group[T](batchId: String, data: Map[AgentId, T]): Map[AgentId, List[T]] = {
    val flat = data.toList.sortBy { case (a, _) => a.value }.map { case (_, r) => r }
    Map(AgentId(batchId) -> flat)
  }

  /**
    * specialized version of ungroup where there is one expected group id
    *
    * @param data data to ungroup
    * @param singleGroupId the group id expected
    * @param agents the agents that are reported in this group
    * @return either the ungrouped data, or an error
    */
  def ungroup[T](
    data: Map[AgentId, List[T]],
    singleGroupId: String,
    agents: List[AgentId]
  ): Either[Error, Map[AgentId, T]] = {
    data.get(AgentId(singleGroupId)) match {
      case None =>
        val keys = data.keys.mkString(",")
        Left(new Error(s"expected group id $singleGroupId not found in response data with keys $keys"))
      case Some(d) if d.lengthCompare(agents) != 0 =>
        Left(new Error(s"data to ungroup doesn't match expected number of agents"))
      case Some(d) =>
        Right(agents.sortBy(_.value).zip(d).toMap)
    }
  }

  /**
    * reverses the grouping action. assumes the agents parameter is the (idempotent) list of all
    * observed batch ids with all agent observations for that batch.
    *
    * @param data data to "ungroup" from the RLlib grouped multiagent shape
    * @param agents the mapping from batch id to all agent ids in that batch
    */
  def ungroup[T](data: Map[AgentId, List[T]], agents: Map[String, List[AgentId]]): Map[AgentId, T] = {
    ???
  }
}
