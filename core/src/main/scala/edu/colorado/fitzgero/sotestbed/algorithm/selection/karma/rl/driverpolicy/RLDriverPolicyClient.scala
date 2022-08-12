package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import java.io.InputStream

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.RLDriverPolicyStructure
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma._
import edu.colorado.fitzgero.sotestbed.rllib.Action

final case class RLDriverPolicyClient(host: String, port: Int, parallelism: Int, trainingEnabled: Boolean) {

  /**
    * helper for sending requests to the RL server for
    * DriverPolicy agents
    */
  def send(reqs: List[PolicyClientRequest]): IO[List[PolicyClientResponse]] =
    PolicyClientOps.send(reqs, host, port, parallelism)

  /**
    * helper for sending requests to the RL server for
    * DriverPolicy agents
    */
  def send(req: PolicyClientRequest): IO[PolicyClientResponse] = PolicyClientOps.send(req, host, port)

}
