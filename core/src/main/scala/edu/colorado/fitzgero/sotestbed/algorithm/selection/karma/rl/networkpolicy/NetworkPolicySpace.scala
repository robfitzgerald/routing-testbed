package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.rllib.Action
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal

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
    *Ma
    * @param zones
    */
  case class ZonalSpeedDelta(zones: List[NetworkZone], aggFn: ObservationAggregation) extends NetworkPolicySpace

  implicit class NPSExtensions(nps: NetworkPolicySpace) {

    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      costFunction: EdgeBPR => Cost
    ): IO[List[Double]] = nps match {
      case ZonalSpeedDelta(zones, aggFn) =>
        zones.traverse { zone =>
          for {
            edges <- network.edges(zone.edges)
          } yield {
            val obs = edges.map { ea => ea.attribute.freeFlowSpeed.value - costFunction(ea.attribute).value }
            aggFn.aggregate(obs)
          }
        }
    }

    def decodeAction(action: Action): IO[NetworkPolicySignal] = ???

  }

}
