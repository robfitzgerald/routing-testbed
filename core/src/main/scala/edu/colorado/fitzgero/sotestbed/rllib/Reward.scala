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

    /**
      * when the RL server is set up as a single agent, we flatten the rewards to
      * a single value
      *
      * @return the reward as a single value (summed if coming from a multiagent reward)
      */
    def toSingleAgentReward: SingleAgentReward = r match {
      case s: SingleAgentReward => s
      case MultiAgentReward(reward) =>
        val r = if (reward.isEmpty) 0.0 else reward.map { case (_, r) => r }.sum
        SingleAgentReward(r)
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
