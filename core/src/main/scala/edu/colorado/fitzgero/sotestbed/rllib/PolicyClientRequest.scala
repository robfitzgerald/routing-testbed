package edu.colorado.fitzgero.sotestbed.rllib

import edu.colorado.fitzgero.sotestbed.util.CirceUtils
import io.circe.CursorOp.DownField
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

sealed trait PolicyClientRequest

object PolicyClientRequest {

  final case class StartEpisodeRequest(episode_id: Option[EpisodeId] = None, training_enabled: Boolean = true)
      extends PolicyClientRequest
  final case class GetActionRequest(episode_id: EpisodeId, observation: Observation) extends PolicyClientRequest

  final case class LogActionRequest(episode_id: EpisodeId, observation: Observation, action: Action)
      extends PolicyClientRequest

  final case class LogReturnsRequest(
    episode_id: EpisodeId,
    reward: Reward,
    info: Map[String, String],
    done: Option[Map[AgentId, Boolean]]
  ) extends PolicyClientRequest
  final case class EndEpisodeRequest(episode_id: EpisodeId, observation: Observation) extends PolicyClientRequest

  implicit class PolicyClientMessageOps(pcm: PolicyClientRequest) {

    def toJson: Json = pcm.asJson.mapObject(_.add("command", Command.jsonEncoder(pcm.command)))

    def command: Command = pcm match {
      case _: StartEpisodeRequest => Command.StartEpisode
      case _: GetActionRequest    => Command.GetAction
      case _: LogActionRequest    => Command.LogAction
      case _: LogReturnsRequest   => Command.LogReturns
      case _: EndEpisodeRequest   => Command.EndEpisode
    }
  }

  implicit val obsMapEnc: Encoder[Map[AgentId, Boolean]] =
    CirceUtils.mapEncoder(_.value, identity)

  implicit val obsMapDec: Decoder[Map[AgentId, Boolean]] =
    CirceUtils.mapDecoder((s: String) => Right(AgentId(s)), (b: Boolean) => Right(b))

  implicit val encodeMessage: Encoder[PolicyClientRequest] =
    Encoder.instance {
      case m: StartEpisodeRequest => m.asJson.mapObject(_.add("command", Command.jsonEncoder(Command.StartEpisode)))
      case m: LogActionRequest    => m.asJson.mapObject(_.add("command", Command.jsonEncoder(Command.LogAction)))
      case m: GetActionRequest    => m.asJson.mapObject(_.add("command", Command.jsonEncoder(Command.GetAction)))
      case m: LogReturnsRequest   => m.asJson.mapObject(_.add("command", Command.jsonEncoder(Command.LogReturns)))
      case m: EndEpisodeRequest   => m.asJson.mapObject(_.add("command", Command.jsonEncoder(Command.EndEpisode)))
    }

  implicit val decodeMessage: Decoder[PolicyClientRequest] =
    Decoder.instance { hcursor =>
      for {
        command <- hcursor.downField("command").as[Command](Command.jsonDecoder)
        message <- command match {
          case Command.ActionSpace      => Left(DecodingFailure("not implemented", List(DownField("command"))))
          case Command.ObservationSpace => Left(DecodingFailure("not implemented", List(DownField("command"))))
          case Command.GetWorkerArgs    => Left(DecodingFailure("not implemented", List(DownField("command"))))
          case Command.GetWeights       => Left(DecodingFailure("not implemented", List(DownField("command"))))
          case Command.ReportSamples    => Left(DecodingFailure("not implemented", List(DownField("command"))))
          case Command.StartEpisode     => hcursor.as[StartEpisodeRequest]
          case Command.GetAction        => hcursor.as[GetActionRequest]
          case Command.LogAction        => hcursor.as[LogActionRequest]
          case Command.LogReturns       => hcursor.as[LogReturnsRequest]
          case Command.EndEpisode       => hcursor.as[EndEpisodeRequest]
        }
      } yield message
    }
}
