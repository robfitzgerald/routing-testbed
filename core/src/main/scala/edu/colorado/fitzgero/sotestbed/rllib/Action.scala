package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Action

object Action {
  case class SingleAgentDiscreteAction(action: Int)                           extends Action
  case class MultiAgentDiscreteAction(action: Map[AgentId, Int])              extends Action
  case class SingleAgentRealAction(action: Double)                            extends Action
  case class MultiAgentRealAction(action: Map[AgentId, Double])               extends Action
  case class GroupedMultiAgentDiscreteAction(action: Map[AgentId, List[Int]]) extends Action
  case class GroupedMultiAgentRealAction(action: Map[AgentId, List[Double]])  extends Action
  // todo: grouped versions

  implicit val obsDiscMapEnc: Encoder[Map[AgentId, Int]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsRealMapEnc: Encoder[Map[AgentId, Double]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsDiscMapDec: Decoder[Map[AgentId, Int]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: Int) => Right(d))

  implicit val obsRealMapDec: Decoder[Map[AgentId, Double]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: Double) => Right(d))

  implicit val obsRealMapQmixDec: Decoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapDecoder(
      (s: String) => Right(AgentId(s)),
      (ds: List[Double]) => {
        ds match {
          case Nil           => Left("empty action list from server")
          case action :: Nil => Right(List(action))
          // we may regret this in the future, it may work against QMIX's assumptions
          case other => Left(s"too many actions sent for agent from server")
        }
      }
    )

  implicit val enc: Encoder[Action] = {
    Encoder.instance {
      case sa: SingleAgentDiscreteAction => sa.action.asJson
      case ma: MultiAgentDiscreteAction  => if (ma.action.isEmpty) None.asJson else ma.action.asJson
      case sa: SingleAgentRealAction     => sa.action.asJson
      case ma: MultiAgentRealAction      => if (ma.action.isEmpty) None.asJson else ma.action.asJson
    }
  }

  implicit val dec: Decoder[Action] =
    List[Decoder[Action]](
      Decoder[Int].map { SingleAgentDiscreteAction.apply }.widen,
      Decoder[Option[Map[AgentId, Int]]].emap {
        case None    => Right(MultiAgentDiscreteAction(Map.empty))
        case Some(m) => Right(MultiAgentDiscreteAction(m))
      },
      Decoder[Double].map { SingleAgentRealAction.apply }.widen,
      Decoder[Option[Map[AgentId, Double]]].emap {
        case None    => Right(MultiAgentRealAction(Map.empty))
        case Some(m) => Right(MultiAgentRealAction(m))
      },
      // rllib sends back actions from QMIX wrapped in a list
      Decoder[Option[Map[AgentId, List[Double]]]].emap {
        case None => Right(MultiAgentRealAction(Map.empty))
        case Some(m) =>
          m.toList
            .traverse {
              case (agent, acts) =>
                acts.headOption match {
                  case None      => Left(s"empty action for agent $agent")
                  case Some(act) => Right((agent, act))
                }
            }
            .map { fixed => MultiAgentRealAction(fixed.toMap) }
      }.widen
    ).reduceLeft(_.or(_))

  implicit class Ext(a: Action) {

    def asSingleAgentDiscreteAction: IO[SingleAgentDiscreteAction] = a match {
      case s: SingleAgentDiscreteAction => IO.pure(s)
      case other                        => IO.raiseError(new Error(s"the action type is not single agent: ${other.getClass.getSimpleName}"))
    }

    def asMultiAgentDiscreteAction: IO[MultiAgentDiscreteAction] = a match {
      case s: MultiAgentDiscreteAction => IO.pure(s)
      case other                       => IO.raiseError(new Error(s"the action type is not multi agent: ${other.getClass.getSimpleName}"))
    }

    def asSingleAgentRealAction: IO[SingleAgentRealAction] = a match {
      case s: SingleAgentRealAction => IO.pure(s)
      case other                    => IO.raiseError(new Error(s"the action type is not single agent: ${other.getClass.getSimpleName}"))
    }

    def asMultiAgentRealAction: IO[MultiAgentRealAction] = a match {
      case s: MultiAgentRealAction => IO.pure(s)
      case other                   => IO.raiseError(new Error(s"the action type is not multi agent: ${other.getClass.getSimpleName}"))
    }
  }

  def extractSingleAgentRealAction(action: Action): IO[Double] =
    action match {
      case SingleAgentRealAction(action) => IO.pure(action)
      case other                         => IO.raiseError(new Error(s"expected SingleAgentRealAction, found $other"))
    }

  def extractMultiAgentRealAction(action: Action): IO[Map[AgentId, Double]] =
    action match {
      case MultiAgentRealAction(action) => IO.pure(action)
      case other                        => IO.raiseError(new Error(s"expected MultiAgentRealAction, found $other"))
    }

  def extractGroupedMultiAgentRealAction(action: Action): IO[Map[AgentId, List[Double]]] =
    action match {
      case GroupedMultiAgentRealAction(action) => IO.pure(action)
      case other                               => IO.raiseError(new Error(s"expected MultiAgentRealAction, found $other"))
    }
}
