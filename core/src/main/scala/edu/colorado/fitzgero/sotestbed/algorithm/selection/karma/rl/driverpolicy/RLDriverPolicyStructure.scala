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

sealed trait RLDriverPolicyStructure

object RLDriverPolicyStructure extends BankOps {

  case class SingleAgentPolicy(space: DriverPolicySpace) extends RLDriverPolicyStructure

  case class MultiAgentPolicy(space: DriverPolicySpace) extends RLDriverPolicyStructure

  implicit class DriverPolicyExt(dp: RLDriverPolicyStructure) {

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
      hist: ActiveAgentHistory,
      bank: Map[String, Karma],
      req: Request,
      paths: List[Path],
      batch: Map[Request, List[Path]]
    ): IO[Observation] = dp match {
      case MultiAgentPolicy(space) => IO.raiseError(new NotImplementedError)
      case SingleAgentPolicy(space) =>
        for {
          balance  <- IO.fromEither(bank.getOrError(req.agent))
          thisHist <- IO.fromEither(hist.observedRouteRequestData.getOrError(req.agent))
          features <- space.encodeObservation(req, balance, thisHist, paths, batch)
        } yield Observation.SingleAgentObservation(features)
    }

    /**
      * decodes an action into a bid for an agent
      *
      * @param action the action to perform
      * @return this action as a [[Karma]] bid or an error if the
      * action does not match the current configurations
      */
    def decodeActionAsBid(
      action: Action
    ): IO[Karma] = {
      dp match {
        case MultiAgentPolicy(space) => IO.raiseError(new NotImplementedError)
        case SingleAgentPolicy(space) =>
          action match {
            case MultiAgentDiscreteAction(action) =>
              IO.raiseError(new IllegalStateException)
            case MultiAgentRealAction(action) =>
              IO.raiseError(new IllegalStateException)
            case SingleAgentDiscreteAction(action) => IO.pure(Karma(action.toLong))
            case SingleAgentRealAction(action)     => IO.pure(Karma(action.toLong))
          }
      }
    }
  }
}
