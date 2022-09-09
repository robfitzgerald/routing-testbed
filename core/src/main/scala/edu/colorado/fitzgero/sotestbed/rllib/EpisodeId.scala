package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}

final case class EpisodeId(value: String) extends AnyVal {
  override def toString(): String = value
}

object EpisodeId {

  /**
    * agent ids are re-used in this testbed when running a repeated
    * experiment, so adding an episode prefix prevents name collisions
    * between episodes for single-agent RL.
    *
    * @param agentId the agent id
    * @param episodePrefix a unique episode prefix
    * @return an EpisodeId for this agent
    */
  def apply(agentId: String, episodePrefix: String): EpisodeId = {
    EpisodeId(f"$episodePrefix-$agentId")
  }

  /**
    * generate an EpisodeId with a unique UUID
    */
  def apply(): EpisodeId = EpisodeId(java.util.UUID.randomUUID.toString)

  implicit val enc: Encoder[EpisodeId] = Encoder[String].contramap { _.value }
  implicit val dec: Decoder[EpisodeId] = Decoder[String].emap { s => Right(EpisodeId(s)) }
}
