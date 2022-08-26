package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}
import cats.syntax.functor._

import io.circe.generic.auto._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.util.CirceUtils

sealed trait Observation

object Observation {
  case class SingleAgentObservation(observation: List[Double])                    extends Observation
  case class MultiAgentObservation(observation: Map[AgentId, List[List[Double]]]) extends Observation

  // def singleAgentObservationHeader(): String = o match {
  //   case sao: SingleAgentObservation => sao.observation.indices.map { i => f"o${i + 1}" }.mkString(",")
  //   case mao: MultiAgentObservation =>
  //     throw new NotImplementedError
  // mao.observation.toList
  //   .sortBy { case (a, _) => a.value }
  //   .zipWithIndex
  //   .map {
  //     case ((a, fs), idx) =>
  //       val features = fs.map { i => f"a${idx}o${i + 1}" }
  //       f"$a," + features
  //   }
  //   .mkString(",")
  // }

  implicit class ObservationOps(o: Observation) {

    def prettyPrint: String = o match {
      case SingleAgentObservation(observation) => observation.asJson.noSpaces
      case MultiAgentObservation(observation) =>
        throw new NotImplementedError
      // observation.toList
      // .sortBy { case (a, _) => a.value }
      // .map { case (a, fs) => }
    }
  }

  implicit val obsMapEnc: Encoder[Map[AgentId, List[List[Double]]]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, List[List[Double]]]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (d: List[List[Double]]) => Right(d))

  implicit val enc: Encoder[Observation] = {
    Encoder.instance {
      case sa: SingleAgentObservation => sa.observation.asJson
      case ma: MultiAgentObservation =>
        if (ma.observation.isEmpty) None.asJson else ma.observation.asJson
    }
  }

  implicit val dec: Decoder[Observation] =
    List[Decoder[Observation]](
      Decoder[List[Double]].map { SingleAgentObservation.apply }.widen,
      Decoder[Option[Map[AgentId, List[List[Double]]]]].emap {
        case None    => Right(MultiAgentObservation(Map.empty))
        case Some(m) => Right(MultiAgentObservation(m))
      }
    ).reduceLeft(_.or(_))
}
