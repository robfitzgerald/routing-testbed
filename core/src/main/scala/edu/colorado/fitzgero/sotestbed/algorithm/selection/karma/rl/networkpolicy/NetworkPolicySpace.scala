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
  case class ZonalSpeedDelta(aggregation: ObservationAggregation) extends NetworkPolicySpace

  implicit class NPSExtensions(nps: NetworkPolicySpace) {

    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      batchZoneLookup: Map[String, List[EdgeId]]
    ): IO[Observation] = nps match {
      case ZonalSpeedDelta(aggregation) =>
        // for each zone, collect the network edge attributes and apply the chosen aggregation function
        val speedByZone = batchZoneLookup.toList.traverse {
          case (batchId, zoneEdges) =>
            for {
              eas <- network.edges(zoneEdges)
            } yield {
              val obs       = eas.map { _.attribute }
              val aggSpeeds = aggregation.aggregate(obs)
              (AgentId(batchId), List(aggSpeeds))
            }
        }
        speedByZone.map { obs => Observation.MultiAgentObservation(obs.toMap) }
    }

    def decodeAction(action: Action, sigGen: NetworkPolicySignalGenerator): IO[Map[String, NetworkPolicySignal]] =
      sigGen.generateSignalsForZones(action)

  }
}
