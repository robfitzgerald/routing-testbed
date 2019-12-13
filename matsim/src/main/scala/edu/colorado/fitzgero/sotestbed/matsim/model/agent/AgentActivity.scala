package edu.colorado.fitzgero.sotestbed.matsim.model.agent
import java.time.LocalTime
import java.time.format.DateTimeFormatter

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.MATSimTextTimeFormat
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Coord

sealed trait AgentActivity {
  def activityType: ActivityType
  def location: EdgeId
  def toXML: xml.Elem
}

object AgentActivity {

  val MATSimTextTimeFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  final case class FirstActivity(
    activityType: ActivityType,
    location: EdgeId,
    coord: Coord,
    endTime: LocalTime
  ) extends AgentActivity {
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value}  x={coord.getX.toString} y={coord.getY.toString} end_time={endTime.format(MATSimTextTimeFormat)}/>
  }

  final case class Activity(
    activityType: ActivityType,
    location: EdgeId,
    coord: Coord,
    startTime: LocalTime,
    endTime: LocalTime,
  ) extends AgentActivity {
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value} x={coord.getX.toString} y={coord.getY.toString} end_time={endTime.format(MATSimTextTimeFormat)}/>
  }

  final case class FinalActivity(
    activityType: ActivityType,
    location: EdgeId,
    coord: Coord
  ) extends AgentActivity {
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value} x={coord.getX.toString} y={coord.getY.toString}/>
  }
}

