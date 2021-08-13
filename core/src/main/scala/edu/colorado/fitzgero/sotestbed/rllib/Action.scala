package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import AgentId._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Action

object Action {
  case class SingleAgentAction(action: Int)              extends Action
  case class MultiAgentAction(action: Map[AgentId, Int]) extends Action

  implicit val obsMapEnc: Encoder[Map[AgentId, Int]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, Int]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: Int) => Right(d))

  implicit val enc: Encoder[Action] = {
    Encoder.instance {
      case sa: SingleAgentAction => sa.action.asJson
      case ma: MultiAgentAction  => if (ma.action.isEmpty) None.asJson else ma.action.asJson
    }
  }

  implicit val dec: Decoder[Action] =
    List[Decoder[Action]](
      Decoder[SingleAgentAction].widen,
      Decoder[Option[Map[AgentId, Int]]].emap {
        case None    => Right(MultiAgentAction(Map.empty))
        case Some(m) => Right(MultiAgentAction(m))
      }
    ).reduceLeft(_.or(_))
}
