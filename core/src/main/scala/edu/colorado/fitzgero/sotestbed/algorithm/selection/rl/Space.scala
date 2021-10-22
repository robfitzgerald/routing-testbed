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
  val DefaultCoordinate = 0.0
  val DefaultCongestion = 0.0
  val DefaultReward     = 0.0

  implicit class SpaceOpsInstance(space: Space) {

    def encodeObservation(costFunction: EdgeBPR => Cost)(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      agents: Map[Request, List[Path]]
    ): Map[AgentId, List[Double]] =
      space match {
        case V1 => SpaceV1Ops.encodeObservation(costFunction)(roadNetwork, agents)
      }

    def defaultObservation: List[Double] = space match {
      case V1 =>
        List(
          DefaultCoordinate,
          DefaultCoordinate,
          DefaultCoordinate,
          DefaultCoordinate,
          DefaultCongestion
        )
    }

    def decodeAction(actions: Map[AgentId, Int], agents: Map[Request, List[Path]]): Map[AgentId, Int] =
      space match {
        case V1 => SpaceV1Ops.decodeAction(actions, agents)
      }

    def computeAgentReward(
      selfish: SelectionCost,
      optimal: SelectionCost,
      agents: Map[Request, List[Path]]
    ): Map[AgentId, Double] =
      space match {
        case V1 => SpaceV1Ops.computeReward(selfish, optimal, agents)
      }

    def defaultReward: Double = space match {
      case V1 => DefaultReward
    }
  }
}
