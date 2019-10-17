package edu.colorado.fitzgero.sotestbed.model.agent

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}

case class Request(agent: String, origin: EdgeId, destination: EdgeId, requestClass: RequestClass, travelMode: TravelMode)

object Request {

//  /**
//    * turns a request into its MATSim XML representation, which is a Leg/Activity Pair
//    * @param roadNetwork underlying graph structure
//    * @param request request data
//    * @return request in xml format
//    */
//  def toXML[F[_], V, E](roadNetwork: RoadNetwork[F, V, E], request: Request): xml.Elem = {
//
//    val endTime: String = request.time.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
//
//    <person id={request.agent}>
//      <attributes>
//        <attribute name="requestClass" class={request.requestClass.toString}></attribute>
//      </attributes>
//      <plan selected="yes">
//          <activity type="home" link={request.origin.toString} end_time={endTime}/>
//        <leg mode="car" dep_time={endTime}></leg>
//        <activity type="work" link={request.destination.toString} end_time={request.time.plusHours(9).toString}/>
//      </plan>
//    </person>
//  }
}
