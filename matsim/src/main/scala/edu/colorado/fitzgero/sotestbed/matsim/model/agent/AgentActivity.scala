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
    coord: Option[Coord],
    endTime: LocalTime
  ) extends AgentActivity {

    def toXML: xml.Elem =
      coord match {
        case None =>
          <activity type={activityType.toString} link={location.value} end_time={endTime.format(MATSimTextTimeFormat)}/>
        case Some(c) =>
          <activity type={activityType.toString} link={location.value}  x={c.getX.toString} y={c.getY.toString} end_time={
            endTime.format(MATSimTextTimeFormat)
          }/>
      }
  }

  final case class Activity(
    activityType: ActivityType,
    location: EdgeId,
    coord: Option[Coord],
    startTime: LocalTime,
    duration: LocalTime
  ) extends AgentActivity {

    def toXML: xml.Elem =
      coord match {
        case None =>
          <activity type={activityType.toString} link={location.value} max_dur={duration.format(MATSimTextTimeFormat)}/>
        case Some(c) =>
          <activity type={activityType.toString} link={location.value}  x={c.getX.toString} y={c.getY.toString} max_dur={
            duration.format(MATSimTextTimeFormat)
          }/>
      }

  }

  final case class FinalActivity(
    activityType: ActivityType,
    location: EdgeId,
    coord: Option[Coord]
  ) extends AgentActivity {

    def toXML: xml.Elem =
      coord match {
        case None => <activity type={activityType.toString} link={location.value}/>
        case Some(c) =>
          <activity type={activityType.toString} link={location.value}  x={c.getX.toString} y={c.getY.toString}/>
      }
  }
}
