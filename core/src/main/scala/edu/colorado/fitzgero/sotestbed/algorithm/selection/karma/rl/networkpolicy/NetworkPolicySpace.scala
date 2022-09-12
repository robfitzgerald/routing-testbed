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

sealed trait NetworkPolicySpace

object NetworkPolicySpace {

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
  case class ZonalSpeedDelta(zones: List[NetworkZone], aggFn: ObservationAggregation) extends NetworkPolicySpace

  implicit class NPSExtensions(nps: NetworkPolicySpace) {

    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR]
    ): IO[Observation] = nps match {
      case ZonalSpeedDelta(zones, aggFn) =>
        val speedByZone = zones.traverse { zone =>
          for {
            edges <- network.edges(zone.edges)
          } yield {
            val speeds    = edges.map(ea => ea.attribute.freeFlowSpeed.value - ea.attribute.observedSpeed.value)
            val aggSpeeds = aggFn.aggregate(speeds)
            (AgentId(zone.zoneId), List(aggSpeeds))
          }
        }
        speedByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
    }

    def decodeAction(action: Action, sigGen: NetworkPolicySignalGenerator): IO[Map[String, NetworkPolicySignal]] =
      sigGen.generateSignalsForZones(action)

  }
}
