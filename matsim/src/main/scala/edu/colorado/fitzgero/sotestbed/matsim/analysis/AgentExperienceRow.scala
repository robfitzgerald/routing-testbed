package edu.colorado.fitzgero.sotestbed.matsim.analysis

import kantan.csv.HeaderDecoder

case class AgentExperienceRow(
  agentId: String,
  requestClass: String,
  departureTime: String,
  travelTime: Double,
  distance: Double,
  replannings: Int
)

object AgentExperienceRow {

  val headerDecoder: HeaderDecoder[AgentExperienceRow] =
    HeaderDecoder.decoder(
      "agentId",
      "requestClass",
      "departureTime",
      "travelTime",
      "distance",
      "replannings"
    ) { AgentExperienceRow.apply }
}
