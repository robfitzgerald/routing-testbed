package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}

final case class EpisodeId(value: String) extends AnyVal

object EpisodeId {
  implicit val enc: Encoder[EpisodeId] = Encoder[String].contramap { _.value }
  implicit val dec: Decoder[EpisodeId] = Decoder[String].emap { s => Right(EpisodeId(s)) }
}
