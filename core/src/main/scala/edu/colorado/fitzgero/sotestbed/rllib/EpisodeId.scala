package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}

final class EpisodeId(private val value: String) extends AnyVal {
  override def toString(): String = value
}

object EpisodeId {
  def apply(agentId: String, episodePrefix: String): EpisodeId = new EpisodeId(f"$episodePrefix-$agentId")
  implicit val enc: Encoder[EpisodeId]                         = Encoder[String].contramap { _.value }
  implicit val dec: Decoder[EpisodeId]                         = Decoder[String].emap { s => Right(new EpisodeId(s)) }
}
