package edu.colorado.fitzgero.sotestbed.matsim.simulator

import scala.collection.JavaConverters._
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
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
import scala.annotation.tailrec
import scala.collection.immutable
import org.matsim.api.core.v01.Scenario

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
  def distanceOfMATSimPath(path: List[Id[Link]], qSim: QSim): Option[Meters] = {
    val distances: List[Double] = for {
      linkId     <- path
      netsimLink <- Try { qSim.getNetsimNetwork.getNetsimLink(linkId) }.toOption
    } yield netsimLink.getLink.getLength
    if (distances.isEmpty) None
    else Some { Meters(distances.sum) }
  }

  /**
    * calculates the total distance of a path (does not validate the path in the process)
    * @param path the path to get distance of
    * @param qSim the network simulation
    * @return an optional distance in Meters
    */
  def distanceOfEdgeData(path: List[EdgeData], qSim: QSim): Option[Meters] = {
    val linkIds = path.map { e => Id.createLinkId(e.edgeId.value) }
    distanceOfMATSimPath(linkIds, qSim)
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
  def convertExperiencedRouteToEdgeData(path: List[(Id[Link], Cost)], qSim: QSim): List[EdgeData] = {

    val result: List[EdgeData] = path.foldLeft(List.empty[EdgeData]) {
      case (acc, (linkId, cost)) =>
        val link                      = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
        val src                       = link.getFromNode.getCoord
        val srcCoordinate: Coordinate = Coordinate(src.getX, src.getY)
        val dst                       = link.getToNode.getCoord
        val dstCoordinate: Coordinate = Coordinate(dst.getX, dst.getY)
        val travelTime: SimTime       = SimTime(cost.value)
        val linkDistance              = link.getLength
        val edgeData: EdgeData = EdgeData(
          EdgeId(linkId.toString),
          srcCoordinate,
          dstCoordinate,
          linkDistance,
          Some { travelTime }
        )
        edgeData +: acc
    }

    result.reverse
  }

  val MinSpeedMph             = 5.0
  val MinSpeedMetersPerSecond = (MinSpeedMph / 3600.0) * 1609.0

  /**
    * this helper takes advantage of the fact that we know
    * the TravelTimeCalculator's implementation of TravelTime.getLinkTravelTime
    * can be passed a null Person and null Vehicle. in that case, it gets the
    * speed un-bounded by limitations of a vehicle. it appears the
    * Person argument is ignored regardless.
    *
    * @param link the link to get speeds for
    * @return the travel time estimate for that
    */
  def getLinkTravelTime(tt: TravelTime, link: Link, currentTime: SimTime, vehicle: Option[Vehicle] = None): SimTime = {
    val veh: Vehicle = vehicle.getOrElse(null)
    val estTime      = tt.getLinkTravelTime(link, currentTime.value, null, veh)
    val estSpeed     = link.getLength / estTime
    val truncSpeed   = math.max(MinSpeedMetersPerSecond, estSpeed)
    val truncTime    = link.getLength / truncSpeed
    val simTime      = SimTime(truncTime)
    if (simTime > SimTime.minute(30)) {
      val forPerson = vehicle.map { v => f"for agent ${v.getId.toString} " }.getOrElse(" ")
      logger.warn(
        f"link ${link.getId}${forPerson}has travel time estimate > 30min. detail: " +
          f"dist ${link.getLength} estTime ${SimTime(estTime)} estSpeed " +
          f"$estSpeed%.2f truncSpeed $truncSpeed%.2f truncTime ${SimTime(truncTime)}"
      )
    }
    simTime
  }

  final case class EdgeDataRequestWithTravelTime(
    person: Person,
    vehicle: Vehicle,
    currentTime: SimTime,
    tt: TravelTime
  ) {

    /**
      * get the travel time for a link as if we were starting to traverse it at the
      * some "current" time
      *
      * doing this because it's not documented how MATSim will interpret times "in the future"
      * passed as the time argument for getLinkTravelTime. it seems like an acceptable assumption
      * as this is only an estimate.
      *
      * note: really weird travel times have been observed, so i've added a min speed to prevent
      * extreme outliers. the default min speed is set above.
      *
      * @param link the link to get time for
      * @return estimated travel time
      */
    def getTravelTime(link: Link): SimTime = {
      getLinkTravelTime(this.tt, link, this.currentTime, Some(this.vehicle))
    }
  }

  def freeFlowTravelTime(
    path: List[Id[Link]],
    qSim: QSim
  ): List[(Id[Link], Double)] = {

    path.map { linkId =>
      val link               = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
      val freeFlowTravelTime = link.getLength / link.getFreespeed
      linkId -> freeFlowTravelTime
    }
  }

  /**
    * takes a planned remaining path and adds Coordinates to each link
    * @param path the route
    * @param qSim the MATSim simulation
    * @param travelTimeRequest optional travel time calculator instance
    * @return EdgeData representation of the experienced path
    */
  def convertRouteToEdgeData(
    path: List[Id[Link]],
    qSim: QSim,
    travelTimeRequest: Option[EdgeDataRequestWithTravelTime] = None
  ): List[EdgeData] = {

    @tailrec
    def _convert(remaining: List[Id[Link]], result: List[EdgeData] = List.empty): List[EdgeData] =
      remaining match {
        case Nil => result
        case head :: tail =>
          val link                      = qSim.getNetsimNetwork.getNetsimLink(head).getLink
          val src                       = link.getFromNode.getCoord
          val srcCoordinate: Coordinate = Coordinate(src.getX, src.getY)
          val dst                       = link.getToNode.getCoord
          val dstCoordinate: Coordinate = Coordinate(dst.getX, dst.getY)
          val linkTravelTime            = travelTimeRequest.map { _.getTravelTime(link) }
          val linkDistance              = link.getLength

          val edgeData = EdgeData(EdgeId(head.toString), srcCoordinate, dstCoordinate, linkDistance, linkTravelTime)

          _convert(tail, edgeData +: result)
      }

    val edgeDataPath = _convert(path).reverse
    edgeDataPath
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
    qSim: QSim
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
    ttRequest: EdgeDataRequestWithTravelTime,
    minimumReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Option[Id[Link]] = {

    // start at the current link
    previousPathFromCurrentOrigin.dropWhile { _ != currentLinkId } match {

      case Nil =>
        // didn't find current edge in agent's route, which is an error; drop this route request.
        None
      case _ :: restOfPath =>
        // find a reasonable start point, starting on the link after the current one.
        // user may be asking for some point of time in the future,
        // but if there isn't enough "slack" remaining in their route for replan,
        // then don't create a request. uses current network travel times.
        val linksWithTimes: List[(Id[Link], SimTime)] = for {
          linkId <- restOfPath
          link = qSim.getNetsimNetwork.getNetsimLink(linkId).getLink
          tt   = ttRequest.getTravelTime(link)
        } yield (linkId, tt)
        val remainingTime =
          if (linksWithTimes.isEmpty) SimTime.Zero
          else SimTime(linksWithTimes.map { case (_, tt) => tt.value }.sum)

        @tailrec
        def _findStartLink(
          remainingLinks: List[(Id[Link], SimTime)],
          remainingTime: SimTime,
          traversalTime: SimTime = SimTime.Zero
        ): Option[Id[Link]] = {
          remainingLinks match {
            case Nil => None
            case (nextLink, nextTT) :: tail =>
              val canReplanHere =
                traversalTime.value > minimumReplanningLeadTime.value &&
                  remainingTime.value > minimumRemainingRouteTimeForReplanning.value
              if (canReplanHere) Some(nextLink)
              else _findStartLink(tail, remainingTime - nextTT, traversalTime + nextTT)
          }
        }

        _findStartLink(linksWithTimes, remainingTime)
    }
  }

  /**
    * tests that, between each pair of links in a path, they share a common node
    *
    * @param path proposed path
    * @param qSim the state of the physical simulation
    * @return true if the path is valid
    */
  def findErrorsInPath(path: List[Id[Link]], qSim: QSim): Option[List[(Id[Link], Id[Link])]] = {

    if (path.lengthCompare(2) < 0) {
      // path is 0 or 1 links, so, it is legal by definition
      None
    } else {

      val network: Network = qSim.getNetsimNetwork.getNetwork

      // for each pair of subsequent links, confirm that the node
      // between them is the same.
      val transitions = for {
        edgePair <- path.sliding(2)
        (linkId1, linkId2) = (edgePair.head, edgePair.last)
        link1              = network.getLinks.get(linkId1)
        link2              = network.getLinks.get(linkId2)
        valid              = link1.getToNode.getId == link2.getFromNode.getId
      } yield {
        (link1.getId, link2.getId, valid)
      }

      // return any link pairs that are not actually adjacent
      transitions.filterNot { case (_, _, valid) => valid }.toList match {
        case Nil          => None
        case invalidPairs => Some(invalidPairs.map { case (s, d, _) => (s, d) })
      }
    }
  }

  /**
    * converts a path from the routing algorithm to a complete MATSim path
    * @param path path response from a routing algorithm
    * @return converted to a MATSim path
    */
  def convertToMATSimPath(path: List[EdgeId]): List[Id[Link]] = {
    path.map { edgeId => Id.createLinkId(edgeId.value) }
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
  def coalescePath(
    oldPath: List[Id[Link]],
    newPath: List[Id[Link]],
    currentLink: Id[Link]
  ): Either[String, List[Id[Link]]] = {
    if (oldPath.isEmpty) {
      Left(s"invariant failed: old path is empty though agent is on network at $currentLink (how did it get there?)")
    } else if (newPath.isEmpty) {
      Left("new path is empty")
    } else if (newPath.last != oldPath.last) {
      Left(s"new path doesn't end at the same destination as the old path (${newPath.last} != ${oldPath.last})")
    } else if (newPath.head == oldPath.head && newPath.last == oldPath.last) {
      // we can swap in this new path entirely
      Right(newPath)
    } else {

      // construct the path prefix using everything in the old path before we see the head of the new path
      oldPath.takeWhile(_ != newPath.head) match {
        case tookTheWholeThing if tookTheWholeThing == oldPath =>
          Left(s"new path origin '${newPath.head}' not found on old path")
        case pathPrefix =>
          val coalescedPath: List[Id[Link]] = pathPrefix ++ newPath
          if (!coalescedPath.contains(currentLink)) {
            Left(
              s"old path and new path when combined at root of new path no longer contain the current link '$currentLink'"
            )
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
