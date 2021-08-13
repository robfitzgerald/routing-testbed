package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Observation

object Observation {
  case class SingleAgentObservation(observation: List[Double])              extends Observation
  case class MultiAgentObservation(observation: Map[AgentId, List[Double]]) extends Observation

  implicit val obsMapEnc: Encoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, List[Double]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[Double]) => Right(d))

  implicit val enc: Encoder[Observation] = {
    Encoder.instance {
      case sa: SingleAgentObservation => sa.observation.asJson
      case ma: MultiAgentObservation =>
        if (ma.observation.isEmpty) None.asJson else ma.observation.asJson
    }
  }

  implicit val dec: Decoder[Observation] =
    List[Decoder[Observation]](
      Decoder[SingleAgentObservation].widen,
      Decoder[Option[Map[AgentId, List[Double]]]].emap {
        case None    => Right(MultiAgentObservation(Map.empty))
        case Some(m) => Right(MultiAgentObservation(m))
      }
    ).reduceLeft(_.or(_))
}
