package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.Action
import edu.colorado.fitzgero.sotestbed.rllib.Action._
import cats.effect._
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

/**
  * at this point we aren't using this abstraction because all
  * network policy signals are multiagent, but in the future,
  * the NetworkPolicySpace would be brokered through this layer
  * that would dictate the agent structure of the problem.
  */
sealed trait NetworkPolicyStructure

object NetworkPolicyStructure {

  case class SingleAgentPolicy(space: NetworkPolicySpace) extends NetworkPolicyStructure

  case class MultiAgentPolicy(space: NetworkPolicySpace) extends NetworkPolicyStructure

  implicit class NPSExtensions(nps: NetworkPolicyStructure) {

    def space: NetworkPolicySpace = nps match {
      case SingleAgentPolicy(space) => space
      case MultiAgentPolicy(space)  => space
    }
  }

}
