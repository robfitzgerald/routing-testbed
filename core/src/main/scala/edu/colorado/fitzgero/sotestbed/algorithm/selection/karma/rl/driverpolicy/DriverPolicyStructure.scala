package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.rllib._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.BankOps
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentRealAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentRealAction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import java.io.File
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

sealed trait DriverPolicyStructure

object DriverPolicyStructure extends BankOps {

  case class SingleAgentPolicy(space: DriverPolicySpace) extends DriverPolicyStructure

  case class MultiAgentPolicy(space: DriverPolicySpace, groupingFile: Option[String]) extends DriverPolicyStructure

  implicit class DriverPolicyExt(dp: DriverPolicyStructure) {

    def space: DriverPolicySpace = dp match {
      case SingleAgentPolicy(space)   => space
      case MultiAgentPolicy(space, _) => space
    }

    /**
      * constructs an observation based on this driver policy. this will
      * either be submitted as a single or multiagent request depending
      * on the policy, and the observation space will be determined by the
      * [[DriverPolicySpace]].
      *
      * @param rn the current road network
      * @param cf a cost function
      * @param hist agent history
      * @param bank the current bank state
      * @param req the request to encode an observation for
      * @param alts the set of alternative paths created for this request
      * @return the effect of creating this observation
      */
    def encodeObservation(
      rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
      hist: ActiveAgentHistory,
      bank: Map[String, Karma],
      req: Request,
      paths: List[Path],
      batch: Map[Request, List[Path]],
      signal: NetworkPolicySignal
    ): IO[List[Double]] =
      for {
        balance  <- IO.fromEither(bank.getOrError(req.agent))
        thisHist <- IO.fromEither(hist.observedRouteRequestData.getOrError(req.agent))
        features <- dp.space.encodeObservation(req, balance, rn, thisHist, paths, batch, signal)
      } yield features

    /**
      * decodes an action into a bid for an agent
      *
      * @param action the action to perform
      * @return this action as a [[Karma]] bid or an error if the
      * action does not match the current configurations
      */
    def decodeSingleAgentActionAsBid(
      action: Action
    ): IO[Karma] = {
      dp match {
        case MultiAgentPolicy(space, _) =>
          IO.raiseError(new Error("provided single agent action, found MultiAgentPolicy"))
        case SingleAgentPolicy(space) =>
          action match {
            case MultiAgentDiscreteAction(action) =>
              IO.raiseError(new Error("expected single agent action, found MultiAgentDiscreteAction"))
            case MultiAgentRealAction(action) =>
              IO.raiseError(new Error("expected single agent action, found MultiAgentRealAction"))
            case SingleAgentDiscreteAction(action) => IO.pure(Karma(action.toLong))
            case SingleAgentRealAction(action)     => IO.pure(Karma(action.toLong))
          }
      }
    }

    def decodeMultiAgentActionAsBid(action: Action): IO[Map[String, Karma]] = {
      val extractedActionResult: IO[Map[String, Karma]] = action match {
        case _: SingleAgentDiscreteAction =>
          IO.raiseError(new Error("expected multiagent action, found SingleAgentDiscreteAction"))
        case _: SingleAgentRealAction =>
          IO.raiseError(new Error("expected multiagent action, found SingleAgentRealAction"))
        case MultiAgentDiscreteAction(action) =>
          IO.pure(action.map { case (k, v) => k.value -> Karma(v.toLong) })
        case MultiAgentRealAction(action) =>
          IO.pure(action.map { case (k, v) => k.value -> Karma(v.toLong) })
      }

      extractedActionResult
    }
  }
}
