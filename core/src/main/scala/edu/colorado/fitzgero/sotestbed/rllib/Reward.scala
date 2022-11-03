package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils
import cats.effect.IO

sealed trait Reward

object Reward {
  case class SingleAgentReward(reward: Double)                           extends Reward
  case class MultiAgentReward(reward: Map[AgentId, Double])              extends Reward
  case class GroupedMultiAgentReward(reward: Map[AgentId, List[Double]]) extends Reward

  implicit val marEnc: Encoder[Map[AgentId, Double]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val marDec: Decoder[Map[AgentId, Double]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: Double) => Right(d))

  implicit val gmarEnc: Encoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val gmarDec: Decoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (ds: List[Double]) => Right(ds))

  implicit val enc: Encoder[Reward] = {
    Encoder.instance {
      case sa: SingleAgentReward        => sa.reward.asJson
      case ma: MultiAgentReward         => if (ma.reward.isEmpty) None.asJson else ma.reward.asJson
      case gma: GroupedMultiAgentReward => if (gma.reward.isEmpty) None.asJson else gma.reward.asJson
    }
  }

  implicit class RewardOps(r: Reward) {

    def prettyPrint: String = r match {
      case SingleAgentReward(reward)       => reward.toString
      case MultiAgentReward(reward)        => reward.asJson.noSpaces
      case GroupedMultiAgentReward(reward) => reward.asJson.noSpaces
    }

    def asSingleAgentReward: IO[SingleAgentReward] = r match {
      case s: SingleAgentReward => IO.pure(s)
      case other                => IO.raiseError(new Error(s"the reward type is not single agent: ${other.getClass.getSimpleName}"))
    }

    def asMultiAgentReward: IO[MultiAgentReward] = r match {
      case s: MultiAgentReward => IO.pure(s)
      case other               => IO.raiseError(new Error(s"the reward type is not multi agent: ${other.getClass.getSimpleName}"))
    }

    def asGroupedMultiAgentReward: IO[GroupedMultiAgentReward] = r match {
      case s: GroupedMultiAgentReward => IO.pure(s)
      case other                      => IO.raiseError(new Error(s"the reward type is not grouped: ${other.getClass.getSimpleName}"))
    }
  }

  implicit val dec: Decoder[Reward] =
    List[Decoder[Reward]](
      Decoder[Double].map { SingleAgentReward.apply }.widen,
      Decoder[Option[Map[AgentId, Double]]].emap {
        case None    => Right(MultiAgentReward(Map.empty[AgentId, Double]))
        case Some(m) => Right(MultiAgentReward(m))
      }
    ).reduceLeft(_.or(_))
}
