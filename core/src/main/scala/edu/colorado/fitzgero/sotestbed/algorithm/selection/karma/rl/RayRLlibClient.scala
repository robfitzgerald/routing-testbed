package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl

import java.io.InputStream

import cats.effect.IO
import cats.implicits._
import io.circe.syntax._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma._
import edu.colorado.fitzgero.sotestbed.rllib.Action
import java.io.PrintWriter

final case class RayRLlibClient(
  host: String,
  port: Int,
  parallelism: Int,
  trainingEnabled: Boolean
) {

  /**
    * helper for sending requests to the RL server for
    * DriverPolicy agents
    */
  def sendMany(
    reqs: List[PolicyClientRequest],
    failOnServerError: Boolean = true,
    logFn: Option[(List[PolicyClientRequest], List[PolicyClientResponse]) => IO[Unit]] = None
  ): IO[List[PolicyClientResponse]] =
    PolicyClientOps.send(reqs, host, port, parallelism, failOnServerError, logFn)

  /**
    * helper for sending requests to the RL server for
    * DriverPolicy agents
    */
  def sendOne(
    req: PolicyClientRequest,
    failOnServerError: Boolean = true,
    logFn: Option[(PolicyClientRequest, PolicyClientResponse) => IO[Unit]] = None
  ): IO[PolicyClientResponse] =
    PolicyClientOps.send(req, host, port, failOnServerError, logFn)

}

object RayRLlibClient {

  def standardSendOneLogFn(pw: PrintWriter): (PolicyClientRequest, PolicyClientResponse) => IO[Unit] =
    (req, res) =>
      IO {
        pw.write(req.asJson.noSpaces.toString + "\n")
        pw.write(res.asJson.noSpaces.toString + "\n")
      }

  def standardSendManyLogFn(pw: PrintWriter): (List[PolicyClientRequest], List[PolicyClientResponse]) => IO[Unit] =
    (reqs, ress) =>
      IO {
        reqs.foreach { req => pw.write(req.asJson.noSpaces.toString + "\n") }
        ress.foreach { res => pw.write(res.asJson.noSpaces.toString + "\n") }
      }

}
