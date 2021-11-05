package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}

sealed trait Command

object Command {
  final case object ActionSpace      extends Command
  final case object ObservationSpace extends Command
  final case object GetWorkerArgs    extends Command
  final case object GetWeights       extends Command
  final case object ReportSamples    extends Command
  final case object StartEpisode     extends Command
  final case object GetAction        extends Command
  final case object LogAction        extends Command
  final case object LogReturns       extends Command
  final case object EndEpisode       extends Command

  implicit class CommandOps(command: Command) {

    /**
      * the string keyword expected in the JSON payload for the given command type
      * @return upper snake case name
      */
    def as_string: String = command.toString.split("(?=[A-Z])").map { _.toUpperCase }.mkString("_")
  }

  val jsonEncoder: Encoder[Command] = Encoder[String].contramap(_.as_string)

  val jsonDecoder: Decoder[Command] = Decoder[String].emap {
    case "ACTION_SPACE"      => Right(ActionSpace)
    case "OBSERVATION_SPACE" => Right(ObservationSpace)
    case "GET_WORKER_ARGS"   => Right(GetWorkerArgs)
    case "GET_WEIGHTS"       => Right(GetWeights)
    case "REPORT_SAMPLES"    => Right(ReportSamples)

    case "START_EPISODE" => Right(StartEpisode)
    case "GET_ACTION"    => Right(GetAction)
    case "LOG_ACTION"    => Right(LogAction)
    case "LOG_RETURNS"   => Right(LogReturns)
    case "END_EPISODE"   => Right(EndEpisode)
    case other           => Left(s"unknown command $other")
  }
}
