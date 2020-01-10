package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import edu.colorado.fitzgero.sotestbed.model.agent.TravelMode
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

final case class AgentActivityPair(
  act1: AgentActivity,
  act2: AgentActivity,
  travelMode: TravelMode
) {

  def homeLocation: Option[EdgeId] = {
    act1 match {
      case AgentActivity.FirstActivity(_, edgeId, _, _) =>
        Some{
          edgeId
        }
      case _ => None
    }
  }
}
