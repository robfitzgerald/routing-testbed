package edu.colorado.fitzgero.sotestbed.matsim.simulator

import scala.collection.JavaConverters._
import scala.util.Try

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.model.agent.RequestClass
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, MetersPerSecond, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.{Link, Network}
import org.matsim.api.core.v01.population.{Leg, Person, Plan}
import org.matsim.core.mobsim.framework.MobsimAgent
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.NetworkRoute

object MATSimRouteOps {

  /**
    * if a path does not include at least two links, then, there is no need to be routed
    * @param path a path
    * @return if it has at least two links, excluding the source and destination
    */
  def completePathHasAtLeastTwoLinks(path: List[Id[Link]]): Boolean = {
    path.nonEmpty && path.tail.drop(1).nonEmpty
  }

  /**
    * converts the Route of a Leg into a list including the source, destination, and middle links
    * @param leg the Leg of a Trip
    * @return a complete route
    */
  def convertToCompleteRoute(leg: Leg): List[Id[Link]] = {
    leg.getRoute.getStartLinkId +:
      leg.getRoute.asInstanceOf[NetworkRoute].getLinkIds.asScala.toList :+
      leg.getRoute.getEndLinkId
  }


  final case class ConvertToRoutingPathAccumulator(
    routingPath: List[AgentBatchData.EdgeData] = List.empty,
    estimatedTravelTime: SimTime = SimTime.Zero
  )

  /**
    * converts a complete route from a Leg into the format used by the routing testbed
    * @param route the complete MATSim route
    * @return as a List[EdgeId]
    */
  def convertToRoutingPath(route: List[Id[Link]], qSim: QSim): List[AgentBatchData.EdgeData] = {
    val result = route.foldLeft(ConvertToRoutingPathAccumulator()) { (acc, linkId) =>
      val link = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
      val src = link.getFromNode.getCoord
      val srcCoordinate = Coordinate(src.getX, src.getY)
      val dst = link.getToNode.getCoord
      val dstCoordinate = Coordinate(dst.getX, dst.getY)
      val nextEstimatedTravelTime: SimTime = SimTime(link.getLength / link.getFreespeed) + acc.estimatedTravelTime
      val edgeData = AgentBatchData.EdgeData(
        EdgeId(linkId.toString),
        nextEstimatedTravelTime,
        srcCoordinate,
        dstCoordinate
      )
      acc.copy(
        routingPath = edgeData +: acc.routingPath,
        estimatedTravelTime = nextEstimatedTravelTime
      )
    }

    result.routingPath.reverse
  }

  /**
    * updates a Leg using a complete route
    * @param links the list of links used by the new route
    * @param leg the Leg we are updating
    */
  def assignCompleteRouteToLeg(links: List[Id[Link]], leg: Leg): Unit = {
    val startEdge              = links.head
    val endEdge                = links.last
    val middle: List[Id[Link]] = links.drop(1).dropRight(1)
    leg.getRoute
      .asInstanceOf[NetworkRoute]
      .setLinkIds(startEdge, middle.asJava, endEdge)
  }

  /**
    * uses link length and free speed to calculate the estimated remaining travel time
    * @param route a route
    * @param currentLinkId the starting link id to calculate to the end of the route
    * @param qSim contains network link data
    * @return the time estimate, in seconds
    */
  def estRemainingTravelTimeSeconds(
    route: List[Id[Link]],
    currentLinkId: Id[Link],
    qSim: QSim,
  ): Double = {
    val lengths: Iterable[Double] = for {
      linkId <- route.dropWhile {
        _ != currentLinkId
      }
      link      = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
      length    = link.getLength
      freespeed = link.getFreespeed
      if freespeed != 0.0
    } yield length / freespeed
    if (lengths.isEmpty) 0.0
    else lengths.sum
  }

  /**
    * accumulator used internally by [[MATSimRouteOps.selectRequestOriginLink]]
    *
    * @param remainingSlack when calculating how far into the future to start replanning, is used to capture
    *                       the distance in time remaining before we can start considering a replanned route
    * @param estimatedRemainingTravelTime the time estimated to traverse the links that we plan to replace
    *                                     with a replanned route
    * @param startPoint the link where the agent is currently located on the path
    * @param pathPrefix links that we will keep which occur between the current link and the beginning of the
    *                   replanned route segment
    */
  private[MATSimRouteOps] final case class ReasonableStartPointFoldAccumulator(
    remainingSlack: TravelTimeSeconds,
    estimatedRemainingTravelTime: TravelTimeSeconds = TravelTimeSeconds.Zero,
    startPoint: Option[Id[Link]] = None,
    pathPrefix: List[Id[Link]] = List.empty
  ) {
    def startPointFound: Boolean           = startPoint.nonEmpty
    def startPointNotFound: Boolean        = startPoint.isEmpty
    def clearedReplanningLeadTime: Boolean = remainingSlack <= TravelTimeSeconds.Zero
  }

  /**
    * finds where in the future to allow a replanning request to originate, or, if no such place exists, returns nothing
    * @param previousPathFromCurrentOrigin path we plan to replace
    * @param currentLinkId the agent's current link location
    * @param destinationLinkId the destination of the path
    * @param qSim contains network link data
    * @param reasonableReplanningLeadTime how many seconds into the future we should allow the agent to continue
    *                                     their currently planned route before injecting a new route plan
    * @param minimumRemainingRouteTimeForReplanning how many seconds there must be between any discovered replanning
    *                                               request origin and the destination in travel time to allow for
    *                                               a replanning origin to be found
    * @return an edge to treat as a new start point, or, nothing
    */
  def selectRequestOriginLink(
    previousPathFromCurrentOrigin: List[Id[Link]],
    currentLinkId: Id[Link],
    destinationLinkId: Id[Link],
    qSim: QSim,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Option[EdgeId] = {

    previousPathFromCurrentOrigin.dropWhile {
      _ != currentLinkId
    } match {
      case Nil =>
        // didn't find current edge in agent's route, which is an error; drop this route request.
        None
      case previousPath =>
        // find a reasonable start point, which may be some point in the future,
        // but if there isn't enough "slack" remaining in their route for replan,
        // then don't touch it.
        val search: ReasonableStartPointFoldAccumulator =
          previousPath
            .map { l =>
              val link: Link = qSim.getNetsimNetwork
                .getNetsimLink(Id.createLinkId(l.toString))
                .getLink
              (l, Meters.toTravelTime(Meters(link.getLength), MetersPerSecond(link.getFreespeed)))
            }
            .foldLeft(MATSimRouteOps.ReasonableStartPointFoldAccumulator(reasonableReplanningLeadTime)) { (acc, tup) =>
              val (linkId, linkTravelTime)              = tup
              val nextRemainingSlack: TravelTimeSeconds = acc.remainingSlack - linkTravelTime
              if (acc.clearedReplanningLeadTime && acc.startPointNotFound) {
                // reasonable start point has been found. store it, and begin storing est. remaining travel time.
                acc.copy(
                  remainingSlack = nextRemainingSlack,
                  estimatedRemainingTravelTime = linkTravelTime,
                  startPoint = Some {
                    linkId
                  }
                )
              } else if (acc.clearedReplanningLeadTime && acc.startPointFound) {
                // reasonable start point was found before. accumulate estimated remaining trip costs
                val nextEstRemainingTravelTime = acc.estimatedRemainingTravelTime + linkTravelTime
                acc.copy(
                  estimatedRemainingTravelTime = nextEstRemainingTravelTime
                )
              } else {
                // searching for reasonable start point. update
                acc.copy(
                  remainingSlack = nextRemainingSlack,
                  pathPrefix = acc.pathPrefix :+ linkId
                )
              }
            }
        for {
          possibleStartPoint <- search.startPoint
          if minimumRemainingRouteTimeForReplanning < search.estimatedRemainingTravelTime
        } yield EdgeId(possibleStartPoint.toString)
    }
  }

  //  def batchEndTimesInRange(startTime: SimTime, endTime: SimTime, batchWindow: SimTime): List[SimTime] = {
  //    val it = Iterator.iterate(startTime.value.toInt) { _ + 1 }.dropWhile { _ % batchWindow.value.toInt != 0 }
  //    if (it.isEmpty) List.empty
  //    else {
  //      {
  //        for {
  //          i <- it.next to endTime.value.toInt by batchWindow.value.toInt
  //        } yield SimTime(i)
  //      }.toList
  //    }
  //  }

  /**
    * tests that, between each pair of links in a path, they share a common node
    *
    * @param path proposed path
    * @param qSim the state of the physical simulation
    * @return true if the path is valid
    */
  def confirmPathIsValid(path: List[Id[Link]], qSim: QSim): Boolean = {

    if (path.lengthCompare(2) < 0) {
      // path is 0 or 1 links, so, it is legal by definition
      true
    } else {

      val network: Network = qSim.getNetsimNetwork.getNetwork

      // for each pair of subsequent links, confirm that the node
      // between them is the same.
      val validTransitions = for {
        edgePair <- path.sliding(2)
        (linkId1, linkId2) = (edgePair.head, edgePair.last)
        link1              = network.getLinks.get(linkId1)
        link2              = network.getLinks.get(linkId2)
      } yield {
        link1.getToNode.getId == link2.getFromNode.getId
      }

      validTransitions.forall(_ == true)
    }
  }

  /**
    * converts a path from the routing algorithm to a complete MATSim path
    * @param path path response from a routing algorithm
    * @return converted to a MATSim path
    */
  def convertToMATSimPath(path: List[EdgeId]): List[Id[Link]] = {
    path.map { edgeId =>
      Id.createLinkId(edgeId.value)
    }
  }

  /**
    * takes the agent's current path, and attaches a new path at the point where they intersect.
    *
    * assuming that the new path has been created based on a link in the old path.
    *
    * @param currentPath the path that MATSim currently has stored for this agent
    * @param newPath     the routing result
    * @return o-[currentPath]->o-[newPath]-> concatenated
    */
  def coalescePath(currentPath: List[Id[Link]], newPath: List[Id[Link]]): List[Id[Link]] = {
    newPath.headOption match {
      case None => currentPath
      case Some(firstLinkInNewPath) =>
        currentPath.takeWhile(_ != firstLinkInNewPath) ++ newPath
    }
  }

  /**
    * a MATSim path does not include start or end. this safely combines everything we have
    * related to this path assignment and turns it into a MATSim path
    *
    * @deprecated
    * @param start         the agent's current location
    * @param end           the destination of the agent's trip
    * @param pathPrefix    a path from the start to a point that the routing algorithm treated as a start point
    * @param routingResult a new path solution to add to this agent, with consideration of the pathPrefix
    * @return all edges between but not including start and end on the new path
    */
  def coalescePath(start: Id[Link], end: Id[Link], pathPrefix: List[Id[Link]], routingResult: List[Id[Link]]): List[Id[Link]] = {
    // if there is a path prefix, it doesn't have the start edge
    val updatedPathPrefix: List[Id[Link]] =
      pathPrefix.headOption match {
        case None => List.empty
        case Some(head) =>
          if (start == head) pathPrefix.tail
          else pathPrefix
      }

    // if there is a routing result, it doesn't have the end edge
    val updatedRoutingResult: List[Id[Link]] =
      routingResult.lastOption match {
        case None => List.empty
        case Some(last) =>
          if (end == last) routingResult.init
          else routingResult
      }

    // combine the prefix and the routing result, making sure not to repeat the joined edge
    val combined: List[Id[Link]] =
      updatedPathPrefix.lastOption match {
        case None => updatedRoutingResult
        case Some(lastLinkInPathPrefix) =>
          updatedRoutingResult.headOption match {
            case None => updatedPathPrefix
            case Some(firstLinkInRoutingResult) =>
              if (lastLinkInPathPrefix == firstLinkInRoutingResult) updatedPathPrefix ++ updatedRoutingResult.tail
              else updatedPathPrefix ++ updatedRoutingResult
          }
      }

    combined
  }

  /**
    * inspects an agent's attributes to determine their request class
    * @deprecated MATSim needs these attributes to be registered; instead, tracking externally
    * @param person a MATSim Person
    * @return their RequestClass, if it exists
    */
  def getRequestClass(person: Person): Option[RequestClass] = {
    val requestClassString: String =
      person.getAttributes.getAttribute("requestclass").asInstanceOf[String]
    RequestClass(requestClassString)
  }

  def safeGetModifiablePlan(mobsimAgent: MobsimAgent): Option[Plan] = {
    Try {
      WithinDayAgentUtils.getModifiablePlan(mobsimAgent)
    }.toOption
  }
}
