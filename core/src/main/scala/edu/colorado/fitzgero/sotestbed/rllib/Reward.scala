package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Reward

object Reward {
  case class SingleAgentReward(reward: Double)              extends Reward
  case class MultiAgentReward(reward: Map[AgentId, Double]) extends Reward

  implicit val obsMapEnc: Encoder[Map[AgentId, Double]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, Double]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: Double) => Right(d))

  implicit val enc: Encoder[Reward] = {
    Encoder.instance {
      case sa: SingleAgentReward => sa.reward.asJson
      case ma: MultiAgentReward  => if (ma.reward.isEmpty) None.asJson else ma.reward.asJson
    }
  }

  implicit class RewardOps(r: Reward) {

    def prettyPrint: String = r match {
      case SingleAgentReward(reward) => reward.toString
      case MultiAgentReward(reward)  => reward.asJson.noSpaces
    }
  }

  implicit val dec: Decoder[Reward] =
    List[Decoder[Reward]](
      Decoder[Double].map { SingleAgentReward.apply }.widen,
      Decoder[Option[Map[AgentId, Double]]].emap {
        case None    => Right(MultiAgentReward(Map.empty))
        case Some(m) => Right(MultiAgentReward(m))
      }
    ).reduceLeft(_.or(_))
}
