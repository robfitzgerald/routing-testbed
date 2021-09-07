package edu.colorado.fitzgero.sotestbed.rllib

import edu.colorado.fitzgero.sotestbed.util.CirceUtils
import edu.colorado.fitzgero.sotestbed.rllib.Action._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.functor._

sealed trait PolicyClientResponse

object PolicyClientResponse {

  final case class StartEpisodeResponse(episode_id: EpisodeId) extends PolicyClientResponse

  final case class GetActionResponse(action: Action) extends PolicyClientResponse

//  final case class LogActionResponse()

//  final case class LogReturnsResponse()

//  final case class EndEpisodeResponse()
  /** .. for each [[PolicyClientRequest]] */
  // JSON response payload should be interpretable from the PolicyServerNoPickleInput file

  implicit val actMapEnc: Encoder[Map[AgentId, Action]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val actMapDec: Decoder[Map[AgentId, Action]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (a: Action) => Right(a))

  implicit val encoder: Encoder[PolicyClientResponse] =
    Encoder.instance {
      case m: StartEpisodeResponse => m.asJson
      case m: GetActionResponse    => m.asJson
    }

  implicit val decoder: Decoder[PolicyClientResponse] =
    List[Decoder[PolicyClientResponse]](
      Decoder[StartEpisodeResponse].widen,
      Decoder[GetActionResponse].widen
    ).reduceLeft { _.or(_) }
}
