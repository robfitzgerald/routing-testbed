package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Reward

object Reward {
  case class SingleAgentReward(reward: Double)                    extends Reward
  case class MultiAgentReward(reward: Map[AgentId, List[Double]]) extends Reward

  implicit val obsMapEnc: Encoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[Double]) => Right(d))

  implicit val enc: Encoder[Reward] = {
    Encoder.instance {
      case sa: SingleAgentReward => sa.reward.asJson
      case ma: MultiAgentReward  => if (ma.reward.isEmpty) None.asJson else ma.reward.asJson
    }
  }

  implicit val dec: Decoder[Reward] =
    List[Decoder[Reward]](
      Decoder[SingleAgentReward].widen,
      Decoder[Option[Map[AgentId, List[Double]]]].emap {
        case None    => Right(MultiAgentReward(Map.empty))
        case Some(m) => Right(MultiAgentReward(m))
      }
    ).reduceLeft(_.or(_))
}
