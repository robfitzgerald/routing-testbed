package edu.colorado.fitzgero.sotestbed.rllib

import io.circe.{Decoder, Encoder}

final case class AgentId(value: String) extends AnyVal

object AgentId {
  implicit val enc: Encoder[AgentId] = Encoder[String].contramap { _.value }
  implicit val dec: Decoder[AgentId] = Decoder[String].emap { s => Right(AgentId(s)) }
}
