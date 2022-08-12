package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Action

object Action {
  case class SingleAgentDiscreteAction(action: Int)                    extends Action
  case class MultiAgentDiscreteAction(action: Map[AgentId, List[Int]]) extends Action
  case class SingleAgentRealAction(action: Double)                     extends Action
  case class MultiAgentRealAction(action: Map[AgentId, List[Double]])  extends Action

  implicit val obsDiscMapEnc: Encoder[Map[AgentId, List[Int]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsRealMapEnc: Encoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsDiscMapDec: Decoder[Map[AgentId, List[Int]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[Int]) => Right(d))

  implicit val obsRealMapDec: Decoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[Double]) => Right(d))

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
      Decoder[Option[Map[AgentId, List[Int]]]].emap {
        case None    => Right(MultiAgentDiscreteAction(Map.empty))
        case Some(m) => Right(MultiAgentDiscreteAction(m))
      },
      Decoder[Double].map { SingleAgentRealAction.apply }.widen,
      Decoder[Option[Map[AgentId, List[Double]]]].emap {
        case None    => Right(MultiAgentRealAction(Map.empty))
        case Some(m) => Right(MultiAgentRealAction(m))
      }
    ).reduceLeft(_.or(_))
}
