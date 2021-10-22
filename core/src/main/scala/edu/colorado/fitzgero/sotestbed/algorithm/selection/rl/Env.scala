package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.Observation.MultiAgentObservation
import edu.colorado.fitzgero.sotestbed.rllib.Reward.MultiAgentReward
import edu.colorado.fitzgero.sotestbed.rllib.{Action, AgentId, Grouping, Observation, Reward}

sealed trait Env

object Env {

  final case class MultiAgentGroupedEnvironment(space: Space, grouping: Grouping) extends Env

  implicit class EnvOpsInstance(env: Env) {

    def encodeObservation(costFunction: EdgeBPR => Cost)(
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      agents: Map[Request, List[Path]]
    ): Either[Error, Observation] = {
      env match {
        case MultiAgentGroupedEnvironment(space, grouping) =>
          // use space + grouping to create observations
          val obsData = space.encodeObservation(costFunction)(roadNetwork, agents)
          for {
            groupedObservations <- grouping.group(obsData)
          } yield MultiAgentObservation(groupedObservations)
      }
    }

    def decodeAction(
      action: Action,
      agents: Map[Request, List[Path]]
    ): Either[Error, Map[AgentId, Int]] = {
      env match {
        case MultiAgentGroupedEnvironment(space, grouping) =>
          action match {
            case Action.MultiAgentDiscreteAction(action) =>
              // use space + grouping to decode action
              for {
                ungroupedActions <- grouping.ungroup(action)
              } yield space.decodeAction(ungroupedActions, agents)

            case _ => Left(new NotImplementedError("only multiagent discrete actions implemented"))
          }
      }

    }

    def encodeReward(
      selfish: SelectionCost,
      optimal: SelectionCost,
      agents: Map[Request, List[Path]]
    ): Either[Error, Reward] = {
      env match {
        case MultiAgentGroupedEnvironment(space, grouping) =>
          // use space + grouping to encode reward
          val rewards = space.computeReward(selfish, optimal, agents)
          for {
            groupedRewards <- grouping.group(rewards)
          } yield MultiAgentReward(groupedRewards)
      }
    }
  }
}
