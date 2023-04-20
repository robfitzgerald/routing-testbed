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
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

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
    */
  final case class ZonalSpeedDelta(observation: ObservationAggregation, reward: ObservationAggregation)
      extends NetworkPolicySpace

  /**
    * encodes a network space where there batchZoneLookup Map[String, List[EdgeId]]
    * has keys which are EdgeIds that represent a single Edge that is a "pinch point"
    * or "choke point" in the network. we observe congestion and rewards only at this
    * location. the Map.values of the zone lookup are the links that are adjacent to
    * the choke point. agents on these links are collected together into a batch.
    */
  final case class ChokePointZones(observation: ObservationAggregation, reward: ObservationAggregation)
      extends NetworkPolicySpace

  final case class FixedSignal(value: Double) extends NetworkPolicySpace

  /**
    * creates network signals that are a direct linear transform of the observation value.
    * uses the function y = mx + b to perform the mapping and limits results to the bounds
    * [lowerBound, upperBound].
    *
    * @param observation how we create observation values for the zone of control
    * @param m slope
    * @param b offset
    * @param upperBound upper limit for values
    * @param lowerBound lower limit for values
    */
  final case class LinearObservationMapping(
    observation: ObservationAggregation,
    m: Double,
    b: Double,
    lowerBound: Double,
    upperBound: Double
  ) extends NetworkPolicySpace

  /**
    * pre-rolled version that transforms to a piecewise linear function that:
    * - doesn't re-route agents when speeds are at least 80% of free flow
    * - linearly increases re-routing as speeds drop between 80% and 20%
    * - re-routes 100% of agents when speeds are 20% or less
    */
  final case object EightyTwentyLinearObservationMapping extends NetworkPolicySpace {
    // arguments for y = mx + b; assumed a normalized observation space in [0, 1]
    val (m, b, lowerBound, upperBound) = (-1.6, 1.3, 0.0, 1.0)
  }

  /**
    * pre-rolled version that transforms to a piecewise linear function that:
    * - doesn't re-route agents when speeds are at least 90% of free flow
    * - linearly increases re-routing as speeds drop between 90% and 50%
    * - re-routes 100% of agents when speeds are 50% or less
    * [[example https://www.desmos.com/calculator/ry0nq8fdad]]
    */
  final case object NinetyFiftyLinearObservationMapping extends NetworkPolicySpace {
    // arguments for y = mx + b; assumed a normalized observation space in [0, 1]
    val (m, b, lowerBound, upperBound) = (-2.5, 2.25, 0.0, 1.0)
  }

  implicit class NPSExtensions(nps: NetworkPolicySpace) {

    // note to self: observations "become" NetworkPolicySignal values directly when the NetworkPolicyConfig
    // is not an external RL server
    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      batchZoneLookup: Map[String, List[EdgeId]]
    ): IO[Observation] = nps match {
      case FixedSignal(value) =>
        val sigs = batchZoneLookup.keys.map { id => (AgentId(id), List(value)) }
        IO.pure(Observation.MultiAgentObservation(sigs.toMap))
      case LinearObservationMapping(observation, m, b, lower, upper) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val obsByZone = batchZoneLookup.toList.traverse {
          case (batchId, zoneEdges) =>
            NetworkAgentAggregationOps
              .applyAggregation(zoneEdges, network, observation)
              .map { x =>
                val y        = (m * x) + b
                val yBounded = math.min(upper, math.max(lower, y))
                (AgentId(batchId), List(yBounded))
              }
        }
        obsByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
      case EightyTwentyLinearObservationMapping =>
        LinearObservationMapping(
          ObservationAggregation.MeanRelativeDistanceWeightedSpeedDiff,
          m = EightyTwentyLinearObservationMapping.m,
          b = EightyTwentyLinearObservationMapping.b,
          lowerBound = EightyTwentyLinearObservationMapping.lowerBound,
          upperBound = EightyTwentyLinearObservationMapping.upperBound
        ).encodeObservation(network, batchZoneLookup)
      case NinetyFiftyLinearObservationMapping =>
        LinearObservationMapping(
          ObservationAggregation.MeanRelativeDistanceWeightedSpeedDiff,
          m = NinetyFiftyLinearObservationMapping.m,
          b = NinetyFiftyLinearObservationMapping.b,
          lowerBound = NinetyFiftyLinearObservationMapping.lowerBound,
          upperBound = NinetyFiftyLinearObservationMapping.upperBound
        ).encodeObservation(network, batchZoneLookup)
      case ZonalSpeedDelta(observation, reward) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val obsByZone = batchZoneLookup.toList.traverse {
          case (batchId, zoneEdges) =>
            NetworkAgentAggregationOps
              .applyAggregation(zoneEdges, network, observation)
              .map { o => (AgentId(batchId), List(o)) }
        }
        obsByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
      case ChokePointZones(observation, reward) =>
        val obsByZone = batchZoneLookup.keys.toList
          .traverse { chokePointEdgeId =>
            for {
              _ <- network.edge(EdgeId(chokePointEdgeId)) // validate batch id **is** an EdgeId
              agg <- NetworkAgentAggregationOps
                .applyAggregation(List(EdgeId(chokePointEdgeId)), network, observation)
                .map { o => (AgentId(chokePointEdgeId), List(o)) }
            } yield agg

          }
        obsByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
    }

    def decodeAction(action: Action, sigGen: NetworkPolicySignalGenerator): IO[Map[String, NetworkPolicySignal]] =
      nps match {
        case _: ZonalSpeedDelta =>
          sigGen.generateSignalsForZones(action)
        case _ =>
          IO.raiseError(new Error(s"decodeAction only defined for ZonalSpeedDelta NetworkPolicySpace"))
      }

    def encodeReward(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      previousZones: Map[String, List[EdgeId]]
    ): IO[Reward] = nps match {
      case ZonalSpeedDelta(observation, reward) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val rewardByZone =
          previousZones.toList
            .traverse {
              case (batchId, zoneEdges) =>
                NetworkAgentAggregationOps
                  .applyAggregation(zoneEdges, network, reward)
                  .map { r => (AgentId(batchId), r) }
            }

        rewardByZone.map { rew => Reward.MultiAgentReward(rew.toMap) }

      case ChokePointZones(observation, reward) =>
        val rewardByZone = previousZones.keys.toList
          .traverse { chokePointEdgeId =>
            for {
              _ <- network.edge(EdgeId(chokePointEdgeId)) // validate batch id **is** an EdgeId
              agg <- NetworkAgentAggregationOps
                .applyAggregation(List(EdgeId(chokePointEdgeId)), network, reward)
                .map { r => (AgentId(chokePointEdgeId), r) }
            } yield agg
          }
        rewardByZone.map { rew => Reward.MultiAgentReward(rew.toMap) }
      case _ =>
        IO.raiseError(new Error(s"encodeReward only defined for ZonalSpeedDelta NetworkPolicySpace"))
    }
  }
}
