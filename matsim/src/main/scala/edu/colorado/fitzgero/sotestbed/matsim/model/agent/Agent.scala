package edu.colorado.fitzgero.sotestbed.matsim.model.agent

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Coord

final case class Agent(
  id: String,
  activities: List[AgentActivityPair]
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

      //        <attributes>
      //          <attribute name="requestClass" class={this.requestClass.toString}/>
      //        </attributes>

      <person id={this.id}>
        <plan selected="yes" score="100">
          {startActivity}{remainingActivities}
        </plan>
      </person>
    }
  }
}

object Agent {

  /**
    * builds an agent that takes a single trip from a source to a destination
    */
  def singleTripAgent(
    agentId: String,
    srcLoc: EdgeId,
    dstLoc: EdgeId,
    departureTime: LocalTime,
    srcCoord: Option[Coord] = None,
    dstCoord: Option[Coord] = None,
    srcActivity: ActivityType = ActivityType.Home,
    dstActivity: ActivityType = ActivityType.Work
  ): Agent = {
    val src = AgentActivity.FirstActivity(ActivityType.Home, srcLoc, srcCoord, departureTime)
    val dst = AgentActivity.FinalActivity(ActivityType.Work, dstLoc, dstCoord)
    Agent(agentId, List(AgentActivityPair(src, dst, TravelMode.Car)))
  }

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
      <activity type={ActivityType.Home.toString} link={homeLocation.value} end_time={
        leaveHomeTimeString.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
      }/>
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
