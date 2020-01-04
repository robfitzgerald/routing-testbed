package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

final case class Agent(
  id: String,
  requestClass: RequestClass,
  activities: List[AgentActivityPair],
) {

  def homeLocation: Option[EdgeId] =
    for {
      firstActivityPair <- activities.headOption
      homeLocation      <- firstActivityPair.homeLocation
    } yield homeLocation

  def firstActivityEndTime: Option[LocalTime] =
    for {
      firstActivityPair <- activities.headOption
      act1 <- firstActivityPair.act1 match {
        case AgentActivity.FirstActivity(_, _, _, endTime) =>
          Some { endTime }
        case _ =>
          None
      }

    } yield {
      act1
    }

  def activitiesToRequests: List[Request] =
    for {
      AgentActivityPair(act1, act2, travelMode) <- activities
    } yield {
      Request(
        id,
        act1.location,
        act2.location,
        requestClass,
        travelMode
      )
    }

  /**
    * convert this [[Agent]] into a <person>
    * @return Agent as an xml tree
    */
  def toXML: Either[Agent.AgentFailure, xml.Elem] = {

//    val selected: String = requestClass match {
//      case RequestClass.UE => "no"
//      case _               => "yes"
//    }

    for {
      startActivity <- Agent.xmlForStartActivity(this)
      remainingActivities = Agent.xmlForRemainingActivities(this)
    } yield {

      <person id={this.id}>
        <attributes>
          <attribute name="requestClass" class={this.requestClass.toString}/>
        </attributes>
        <plan>
          {startActivity}{remainingActivities}
        </plan>
      </person>
    }
  }
}

object Agent {

  sealed trait AgentFailure
  final case class MissingHomeLocation(id: String) extends AgentFailure

  def xmlForStartActivity(agent: Agent): Either[AgentFailure, xml.Elem] = {

    val leaveHomeTimeString: String =
      agent.firstActivityEndTime
        .getOrElse(LocalTime.parse("23:59:59"))
        .format(DateTimeFormatter.ofPattern("HH:mm:ss"))

    for {
      homeLocation <- agent.homeLocation.toRight(Agent.MissingHomeLocation(agent.id))
    } yield {
      <activity type={ActivityType.Home.toString} link={homeLocation.value} end_time={leaveHomeTimeString.format(DateTimeFormatter.ofPattern("HH:mm:ss"))}/>
    }
  }

  def xmlForRemainingActivities(agent: Agent): List[xml.Elem] = {
    for {
      actPair <- agent.activities
    } yield {
      List(
        <leg mode={actPair.travelMode.toString}/>,
        actPair.act2.toXML
      )
    }
  }.flatten
}
