package edu.colorado.fitzgero.sotestbed.matsim.simulator

import scala.collection.JavaConverters._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData.EdgeData
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Meters, MetersPerSecond, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import org.matsim.api.core.v01.{Coord, Id}
import org.matsim.api.core.v01.network.{Link, Network}
import org.matsim.api.core.v01.population.{Activity, Leg, Person, Plan}
import org.matsim.core.mobsim.framework.{MobsimAgent, PlayPauseSimulationControl}
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.{NetworkRoute, RouteUtils}
import org.matsim.core.router.util.TravelTime
import org.matsim.vehicles.Vehicle
import org.matsim.withinday.trafficmonitoring.WithinDayTravelTime

object MATSimRouteOps extends LazyLogging {

  var unableToRetrieveModifiableLeg: Int = 0

  /**
    * MATSim's getModifiableCurrentLeg can throw an exception when the agent's current link
    * pointer is set to -1. Legs can also be null.
    * @param mobsimAgent the agent whos current Leg we are interested in
    * @return the Optional Leg
    */
  def safeGetModifiableLeg(mobsimAgent: MobsimAgent): Option[Leg] = {
    Try {
      WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)
    } match {
      case util.Success(nullableLeg) =>
        if (nullableLeg == null) {
          unableToRetrieveModifiableLeg += 1
          logger.warn(s"attempted to retrieve modifiable leg for agent ${mobsimAgent.getId} but got null")
          None
        } else {
          Some { nullableLeg }
        }
      case util.Failure(e) =>
        unableToRetrieveModifiableLeg += 1
        logger.warn(s"failed to retrieve modifiable leg for agent ${mobsimAgent.getId} due to: ${e.getMessage}")
        None
    }
  }

  /**
    * calculates the total distance of a path (does not validate the path in the process)
    * @param path the path to get distance of
    * @param qSim the network simulation
    * @return an optional distance in Meters
    */
  def distanceOfPath(path: List[Id[Link]], qSim: QSim): Option[Meters] = {
    val distances: List[Double] = for {
      linkId     <- path
      netsimLink <- Try { qSim.getNetsimNetwork.getNetsimLink(linkId) }.toOption
    } yield netsimLink.getLink.getLength
    if (distances.isEmpty) None
    else Some { Meters(distances.sum) }
  }

  /**
    * steps through an agent's [[Plan]] to find a [[Leg]] with a matching [[DepartureTime]]
    * @param plan the agent's plan
    * @param departureTime the time that we expect to match a Leg's departure time
    * @return [[Some]] expected [[Leg]], or [[None]]
    */
  def getLegFromPlanByDepartureTime(plan: Plan, departureTime: DepartureTime): Option[Leg] = {
    plan.getPlanElements.asScala.toList
      .find { l =>
        l.isInstanceOf[Leg] && DepartureTime.getLegDepartureTime(l.asInstanceOf[Leg]).contains(departureTime)
      }
      .map { _.asInstanceOf[Leg] }
  }

  /**
    * gets the current leg
    * @param mobsimAgent the mobsim agent
    * @return [[Some]] expected [[Leg]], or [[None]]
    */
  def getCurrentLegFromPlan(mobsimAgent: MobsimAgent): Option[Leg] = {
    val idx: Int = WithinDayAgentUtils.getCurrentPlanElementIndex(mobsimAgent)
    if (idx == -1) None // haven't begun our trip yet!
    else {
      safeGetModifiablePlan(mobsimAgent).flatMap { plan =>
        plan.getPlanElements.asScala.toList(idx) match {
          case leg: Leg               => Some { leg }
          case _: Activity if idx > 0 =>
            // maybe we just started the next activity, so, let's see if idx - 1 is a leg
            plan.getPlanElements.asScala.toList(idx - 1) match {
              case _: Activity => None // nonsense
              case leg: Leg    => Some { leg }
            }
        }
      }
    }
  }

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

  /**
    * takes an experienced path and adds Coordinates to each link
    * @param path the route
    * @param qSim the MATSim simulation
    * @return EdgeData representation of the experienced path
    */
  def convertExperiencedRouteToEdgeData(path: List[(Id[Link], Cost)], qSim: QSim): List[RouteRequestData.EdgeData] = {

    val result: List[EdgeData] = path.foldLeft(List.empty[EdgeData]) {
      case (acc, (linkId, cost)) =>
        val link                      = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
        val src                       = link.getFromNode.getCoord
        val srcCoordinate: Coordinate = Coordinate(src.getX, src.getY)
        val dst                       = link.getToNode.getCoord
        val dstCoordinate: Coordinate = Coordinate(dst.getX, dst.getY)
        val travelTime: SimTime       = SimTime(cost.value)
        val edgeData: RouteRequestData.EdgeData = RouteRequestData.EdgeData(
          EdgeId(linkId.toString),
          srcCoordinate,
          dstCoordinate,
          Some { travelTime }
        )
        edgeData +: acc
    }

    result.reverse
  }

  /**
    * takes a planned remaining path and adds Coordinates to each link
    * @param path the route
    * @param qSim the MATSim simulation
    * @return EdgeData representation of the experienced path
    */
  def convertRouteToEdgeData(path: List[Id[Link]], qSim: QSim): List[RouteRequestData.EdgeData] = {

    val result: List[EdgeData] = path.foldLeft(List.empty[EdgeData]) {
      case (acc, linkId) =>
        val link                      = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
        val src                       = link.getFromNode.getCoord
        val srcCoordinate: Coordinate = Coordinate(src.getX, src.getY)
        val dst                       = link.getToNode.getCoord
        val dstCoordinate: Coordinate = Coordinate(dst.getX, dst.getY)
        val edgeData: RouteRequestData.EdgeData = RouteRequestData.EdgeData(
          EdgeId(linkId.toString),
          srcCoordinate,
          dstCoordinate
        )
        edgeData +: acc
    }

    result.reverse
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
    * @param minimumReplanningLeadTime how many seconds into the future we should allow the agent to continue
    *                                     their currently planned route before injecting a new route plan
    * @param minimumRemainingRouteTimeForReplanning how many seconds there must be between any discovered replanning
    *                                               request origin and the destination in travel time to allow for
    *                                               a replanning origin to be found
    * @return an edge to treat as a new start point (at least the next link), or, nothing
    */
  def selectRequestOriginLink(
    previousPathFromCurrentOrigin: List[Id[Link]],
    currentLinkId: Id[Link],
    destinationLinkId: Id[Link],
    qSim: QSim,
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Option[EdgeId] = {

    // start at the current link
    previousPathFromCurrentOrigin.dropWhile { _ != currentLinkId } match {

      case Nil =>
        // didn't find current edge in agent's route, which is an error; drop this route request.
        None
      case _ :: restOfPath =>
        // find a reasonable start point, starting on the link after the current one.
        // user may be asking for some point of time in the future,
        // but if there isn't enough "slack" remaining in their route for replan,
        // then don't touch it.
        val search: ReasonableStartPointFoldAccumulator =
          restOfPath
            .map { l =>
              val link: Link = qSim.getNetsimNetwork
                .getNetsimLink(Id.createLinkId(l.toString))
                .getLink
              (l, Meters.toTravelTime(Meters(link.getLength), MetersPerSecond(link.getFreespeed)))
            }
            .foldLeft(MATSimRouteOps.ReasonableStartPointFoldAccumulator(minimumReplanningLeadTime)) { (acc, tup) =>
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
                val nextEstRemainingTravelTime: TravelTimeSeconds = acc.estimatedRemainingTravelTime + linkTravelTime
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

        val result: Option[EdgeId] = for {
          possibleStartPoint <- search.startPoint
          if minimumRemainingRouteTimeForReplanning < search.estimatedRemainingTravelTime
        } yield EdgeId(possibleStartPoint.toString)

        result
    }
  }

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
    * accumulates a new path by adding links to itself and tracking details
    * which are used to determine the validity of this route.
    *
    * note: uses List prepend (reverse) ordering for performance
    * @param currentLink the agent's current location
    * @param replanningLink the link where the new path begins
    * @param coalescedRoutePrependOrdered the updated route
    * @param currentLinkIdxSeen any indices of this coalesced route which match the currentLink
    * @param replanningLinkIdxSeen any indices of this coalesced route which match the replanningLink
    * @param pathLength the length of the path
    */
  final case class CoalesceAccumulator(
    currentLink: Id[Link],
    replanningLink: Id[Link],
    coalescedRoutePrependOrdered: List[Id[Link]] = List.empty,
    currentLinkIdxSeen: Option[List[Int]] = None,
    replanningLinkIdxSeen: Option[List[Int]] = None,
    pathLength: Int = 0
  ) {

    /**
      * add one more link, updating the accumulator
      * @param link the new link
      * @return the updated accumulator
      */
    def addLink(link: Id[Link]): CoalesceAccumulator = {

      val nextCurrentLinkIdxSeen: Option[List[Int]] =
        if (link == currentLink) {
          this.currentLinkIdxSeen match {
            case None     => Some { List(this.pathLength) }
            case Some(xs) => Some { this.pathLength +: xs }
          }
        } else {
          this.currentLinkIdxSeen
        }

      val nextReplanningLinkIdxSeen: Option[List[Int]] =
        if (link == replanningLink) {
          this.replanningLinkIdxSeen match {
            case None     => Some { List(this.pathLength) }
            case Some(xs) => Some { this.pathLength +: xs }
          }
        } else {
          this.replanningLinkIdxSeen
        }

      this.copy(
        coalescedRoutePrependOrdered = link +: this.coalescedRoutePrependOrdered,
        currentLinkIdxSeen = nextCurrentLinkIdxSeen,
        replanningLinkIdxSeen = nextReplanningLinkIdxSeen,
        pathLength = this.pathLength + 1
      )
    }

    /**
      * reviews accumulated data and returns either the new route, or, an error
      * @return the new path, or, an error
      */
    def result: Either[String, List[Id[Link]]] = {
      for {
        currentLinkIdx    <- currentLinkIdxSeen.toRight("current link not seen")
        replanningLinkIdx <- replanningLinkIdxSeen.toRight("replanning link not seen")
      } yield {
        val currentLinkOccursAtLeastOnce: Boolean    = currentLinkIdx.lengthCompare(1) >= 0
        val currentLinkOccursAtMostTwice: Boolean    = currentLinkIdx.lengthCompare(2) <= 0
        val replanningLinkOccursExactlyOnce: Boolean = replanningLinkIdx.lengthCompare(1) == 0
        val correctOrder: Boolean                    = currentLinkIdx.last < replanningLinkIdx.last

        if (!(currentLinkOccursAtLeastOnce && currentLinkOccursAtMostTwice)) {
          (Some { "current link missing or occurred more than twice in updated path" }, List.empty)
        } else if (!replanningLinkOccursExactlyOnce) {
          (Some { "replanning link doesn't appear exactly once in updated path" }, List.empty)
        } else if (!correctOrder) {
          (Some { "replanning link came before current link in updated path" }, List.empty)
        } else {
          (Option.empty[String], coalescedRoutePrependOrdered.reverse)
        }
      }
    } match {
      case Left(msg) =>
        println(msg)
        Left(msg)
      case Right((badPathOption, path)) =>
        badPathOption match {
          case None => Right(path)
          case Some(badPath) =>
            println(badPath)
            Left(badPath)
        }
    }
  }

  /**
    * takes the agent's current path, and attaches a new path at the point where they intersect.
    *
    * assuming that the new path has been created based on a link in the old path.
    *
    * invariant: the current link should occur at the same index as it did in the previous path
    *
    * @param oldPath     the path that MATSim currently has stored for this agent
    * @param newPath     the routing result
    * @param currentLink the agent's current link
    * @return o-[currentPath]->o-[newPath]-> concatenated, or a message why that failed; along with the index of the current link
    */
  def coalescePath(oldPath: List[Id[Link]], newPath: List[Id[Link]], currentLink: Id[Link]): Either[String, List[Id[Link]]] = {
    if (oldPath.isEmpty) {
      Left(s"invariant failed: old path is empty though agent is on network at $currentLink (how did it get there?)")
    } else if (newPath.isEmpty) {
      Left("new path is empty")
    } else {

      // construct the path prefix using everything in the old path before we see the head of the new path
      oldPath.takeWhile(_ != newPath.head) match {
        case Nil =>
          Left(s"new path origin ${newPath.head} not found on old path")
        case pathPrefix =>
          val coalescedPath: List[Id[Link]] = pathPrefix ++ newPath
          if (!coalescedPath.contains(currentLink)) {
            Left("old path and new path when combined at root of new path no longer contain the current link")
          } else if (coalescedPath.lengthCompare(coalescedPath.toSet.size) != 0) {
            Left("coalesced path contains a loop")
          } else {
            Right(coalescedPath)
          }
      }
    }
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
