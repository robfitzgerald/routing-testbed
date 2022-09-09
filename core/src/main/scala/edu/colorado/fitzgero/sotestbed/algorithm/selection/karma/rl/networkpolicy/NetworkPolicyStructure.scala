package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.Action
import edu.colorado.fitzgero.sotestbed.rllib.Action._
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

sealed trait NetworkPolicyStructure

object NetworkPolicyStructure {

  case class SingleAgentPolicy(space: NetworkPolicySpace) extends NetworkPolicyStructure

  case class MultiAgentPolicy(space: NetworkPolicySpace) extends NetworkPolicyStructure

  implicit class NPSExtensions(nps: NetworkPolicyStructure) {

    def space: NetworkPolicySpace = nps match {
      case SingleAgentPolicy(space) => space
      case MultiAgentPolicy(space)  => space
    }

    def encodeObservation(
      network: RoadNetwork[IO, Coordinate, EdgeBPR],
      costFunction: EdgeBPR => Cost
    ): IO[List[Double]] = nps.space.encodeObservation(network, costFunction)

    def decodeAction(action: Action): IO[Unit] = IO.unit

  }

}
