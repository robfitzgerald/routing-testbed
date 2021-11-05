package edu.colorado.fitzgero.sotestbed.rllib

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.util.CirceUtils
import edu.colorado.fitzgero.sotestbed.rllib.Action._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.functor._

sealed trait PolicyClientResponse

object PolicyClientResponse {

  final case class StartEpisodeResponse(episode_id: EpisodeId) extends PolicyClientResponse
  final case class GetActionResponse(action: Action)           extends PolicyClientResponse
  final case object Empty                                      extends PolicyClientResponse

  implicit class ResponseOps(res: PolicyClientResponse) {

    def getEpisodeId: IO[EpisodeId] = res match {
      case StartEpisodeResponse(episode_id) => IO.pure(episode_id)
      case other                            => IO.raiseError(new scala.Error(s"expected response with EpisodeId, got $other"))
    }

    def getAction: IO[Action] = res match {
      case GetActionResponse(action) => IO.pure(action)
      case other                     => IO.raiseError(new scala.Error(s"expected response with Action, got $other"))
    }
  }

  implicit val actMapEnc: Encoder[Map[AgentId, Action]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val actMapDec: Decoder[Map[AgentId, Action]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (a: Action) => Right(a))

  implicit val encoder: Encoder[PolicyClientResponse] =
    Encoder.instance {
      case m: StartEpisodeResponse => m.asJson
      case m: GetActionResponse    => m.asJson
      case Empty                   => "{}".asJson
    }

  implicit val decoder: Decoder[PolicyClientResponse] =
    List[Decoder[PolicyClientResponse]](
      Decoder[StartEpisodeResponse].widen,
      Decoder[GetActionResponse].widen,
      Decoder[String].emap {
        _.trim.toLowerCase match {
          case "{}"  => Right(Empty)
          case other => Left(f"unexpected response $other")
        }
      }
    ).reduceLeft { _.or(_) }
}
