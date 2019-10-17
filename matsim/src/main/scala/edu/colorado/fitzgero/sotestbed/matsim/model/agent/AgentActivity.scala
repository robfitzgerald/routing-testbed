package edu.colorado.fitzgero.sotestbed.matsim.model.agent
import java.time.LocalTime
import java.time.format.DateTimeFormatter

import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity.MATSimTextTimeFormat
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

sealed trait AgentActivity {
  def activityType: ActivityType
  def location: EdgeId
  def toXML: xml.Elem
}

// TODO:
//  Leg departure time is internal; we don't need to figure it out.
//  all we need is for activities to have end_times, except final
//  activities.
//  FIX IT YO! redraw, redo, refactor, recompoop.


trait HasDepartureTime {
  def departureTime: LocalTime
}

object AgentActivity {

  val MATSimTextTimeFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  final case class FirstActivity(
    activityType: ActivityType,
    location: EdgeId,
    endTime: LocalTime
  ) extends AgentActivity {
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value} end_time={endTime.format(MATSimTextTimeFormat)}/>
  }

  final case class Activity(
    activityType: ActivityType,
    location: EdgeId,
    departureTime: LocalTime, // TODO: comes from road network path estimate during Agent construction
    startTime: LocalTime,
    endTime: LocalTime,
  ) extends AgentActivity with HasDepartureTime {
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value} end_time={endTime.format(MATSimTextTimeFormat)}/>
  }

  final case class FinalActivity(
    activityType: ActivityType,
    location: EdgeId,
    departureTime: LocalTime
  ) extends AgentActivity with HasDepartureTime{
    def toXML: xml.Elem = <activity type={activityType.toString} link={location.value}/>
  }
}

