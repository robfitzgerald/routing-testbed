package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.AgentId

sealed trait Space

object Space {

  final case object V1 extends Space

  implicit class SpaceOpsInstance(space: Space) {

    def encodeObservation(costFunction: EdgeBPR => Cost)(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      agents: Map[Request, List[Path]]
    ): Map[AgentId, List[Double]] =
      space match {
        case V1 => SpaceV1Ops.encodeObservation(costFunction)(roadNetwork, agents)
      }

    def decodeAction(actions: Map[AgentId, Int], agents: Map[Request, List[Path]]): Map[AgentId, Int] =
      space match {
        case V1 => SpaceV1Ops.decodeAction(actions, agents)
      }

    def computeReward(
      selfish: SelectionCost,
      optimal: SelectionCost,
      agents: Map[Request, List[Path]]
    ): Map[AgentId, Double] =
      space match {
        case V1 => SpaceV1Ops.computeReward(selfish, optimal, agents)
      }
  }
}
