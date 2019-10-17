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

  def leaveHomeTime: Option[LocalTime] = {
    act1 match {
      case _: AgentActivity.FirstActivity =>
        act2 match {
          case hasDepartureTime: HasDepartureTime =>
            Some{
              hasDepartureTime.departureTime
            }
        }
      case _ => None
    }
  }

  def homeLocation: Option[EdgeId] = {
    act1 match {
      case AgentActivity.FirstActivity(_, location, _) =>
        Some{
          location
        }
      case _ => None
    }
  }
}
