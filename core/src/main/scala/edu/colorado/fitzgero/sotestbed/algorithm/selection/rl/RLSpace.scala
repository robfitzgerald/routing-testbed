package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.config.MarginalCostFunctionConfig
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.{Action, Observation, Reward}

sealed trait RLSpace

object RLSpace {

  type ObservationFunction = (RoadNetwork[IO, Coordinate, EdgeBPR], Map[Request, List[Path]]) => Observation
  type DecodeAction        = (Action, Map[Request, List[Path]]) => List[Int]
  type RewardFunction      = (SelectionCost, SelectionCost, Map[Request, List[Path]]) => Reward

  case class V1(mcf: MarginalCostFunctionConfig) extends RLSpace

  implicit class SpaceOps(space: RLSpace) {

    def getObservationFunctionFn: ObservationFunction =
      space match {
        case V1(mcf) =>
          val cf  = (edge: EdgeBPR) => mcf.build()(edge)(Flow.Zero)
          val enc = SpaceV1.encodeObservation(cf) _
          enc
      }

    def getDecodeActionFn: DecodeAction =
      space match {
        case V1(_) => SpaceV1.decodeAction
      }

    def getRewardFn: RewardFunction =
      space match {
        case V1(_) => SpaceV1.computeReward
      }
  }

}
