package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.rllib.Action
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentRealAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentRealAction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignalGenerator
import edu.colorado.fitzgero.sotestbed.rllib.AgentId
import edu.colorado.fitzgero.sotestbed.rllib.Observation
import edu.colorado.fitzgero.sotestbed.rllib.Reward

sealed trait NetworkPolicySpace

object NetworkPolicySpace {

  // maybe a future extension here...
  // case object UseAgentLocations - only aggregate from Request.location edges
  // case object UseCompleteZone - all values for a BatchId

  /**
    * encodes the network space so that, for each zone, we capture
    * a single value which is the difference in speed between freeflow
    * speed and the current speed. the resulting speed delta value
    * is 0 if the speed is currently free flow, and less if slower, in
    * meters per second.
    *
    * this assumes the CostFunction provided when encoding produces Costs
    * which are also in Meters Per Second.
    *
    * @param zones
    */
  case class ZonalSpeedDelta(observation: ObservationAggregation, reward: ObservationAggregation)
      extends NetworkPolicySpace

  implicit class NPSExtensions(nps: NetworkPolicySpace) {

    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      batchZoneLookup: Map[String, List[EdgeId]]
    ): IO[Observation] = nps match {
      case ZonalSpeedDelta(observation, reward) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val obsByZone = batchZoneLookup.toList.traverse {
          case (batchId, zoneEdges) =>
            for {
              eas <- network.edges(zoneEdges)
            } yield {
              val obs       = eas.map { _.attribute }
              val aggSpeeds = observation.aggregate(obs)
              (AgentId(batchId), List(aggSpeeds))
            }
        }
        obsByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
    }

    def decodeAction(action: Action, sigGen: NetworkPolicySignalGenerator): IO[Map[String, NetworkPolicySignal]] =
      sigGen.generateSignalsForZones(action)

    def encodeReward(
      batchIds: List[String],
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      batchZoneLookup: Map[String, List[EdgeId]]
    ): IO[Reward] = nps match {
      case ZonalSpeedDelta(observation, reward) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val batchIdsSet = batchIds.toSet
        val rewardByZone =
          batchZoneLookup
            .filter { case (bId, _) => batchIdsSet.contains(bId) }
            .toList
            .traverse {
              case (batchId, zoneEdges) =>
                for {
                  eas <- network.edges(zoneEdges)
                } yield {
                  val edges = eas.map { _.attribute }
                  val r     = reward.aggregate(edges)
                  (AgentId(batchId), r)
                }
            }

        rewardByZone.map { obs => Reward.MultiAgentReward(obs.toMap) }
    }

    def encodeFinalObservation(batchIds: List[String]): IO[Observation] = nps match {
      case ZonalSpeedDelta(observation, reward) =>
        observation.finalObservation.map { fo =>
          val r = batchIds.map { id => AgentId(id) -> List(fo) }.toMap
          Observation.MultiAgentObservation(r)
        }
    }

    def encodeFinalReward(batchIds: List[String]): IO[Reward] = nps match {
      case ZonalSpeedDelta(observation, reward) =>
        reward.finalReward.map { fw =>
          val r = batchIds.map { id => AgentId(id) -> fw }.toMap
          Reward.MultiAgentReward(r)
        }
    }

  }
}
