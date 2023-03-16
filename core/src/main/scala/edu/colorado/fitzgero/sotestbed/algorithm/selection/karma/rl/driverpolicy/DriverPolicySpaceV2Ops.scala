package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import scala.collection.immutable
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.PathSegment
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import com.typesafe.scalalogging.LazyLogging

object DriverPolicySpaceV2Ops extends LazyLogging {

  // a couple of things to keep in mind here
  // - an experienced route has it's travel times recorded in the history on the edges
  // - a remaining route has it's travel time estimates recorded on the edges
  // - the currently active link exists.. (?)
  // - if a remaining route was recorded in the past, we may want to update the costs
  //   by sampling the RoadNetwork to update them

  def getUoPathAlternative(req: Request, alts: Map[Request, List[Path]]): IO[Path] =
    for {
      paths  <- IO.fromOption(alts.get(req))(new Error(s"alts missing agent ${req.agent}"))
      uoPath <- IO.fromOption(paths.headOption)(new Error(s"alts missing paths ${req.agent}"))
    } yield uoPath

  def getSoPathAlternative(req: Request, alts: Map[Request, List[Path]]): IO[Path] =
    for {
      paths  <- IO.fromOption(alts.get(req))(new Error(s"alts missing agent ${req.agent}"))
      soPath <- IO.fromOption(paths.lastOption)(new Error(s"alts missing paths ${req.agent}"))
    } yield soPath

  def currentRoute(history: AgentHistory): IO[List[EdgeData]] =
    IO.fromEither(history.currentRequest.map { _.route })

  def currentTripTravelTimeEstimate(history: AgentHistory): IO[Double] =
    for {
      current <- IO.fromEither(history.currentRequest)
      overall <- IO.fromEither(current.overallTravelTimeEstimate)
    } yield overall.value

  /**
    * attempts to compute the delay offset with a whole lot more carefulness around making
    * sure we are making an apples-to-apples comparison.
    *
    * @param rn
    * @param hist
    * @param request
    * @param paths
    * @return
    */
  def delayOffset(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    hist: ActiveAgentHistory,
    request: Request,
    paths: List[Path]
  ): IO[Double] = {
    paths match {
      case newPath :: oldPath :: _ =>
        for {
          forkEdgeId             <- grabForkEdgeId(newPath, oldPath)
          newEdgeData            <- newPath.traverse(_.toEdgeDataWithUpdatedCost(rn)).map(_.dropRight(1).drop(1))
          oldEdgeData            <- oldPath.traverse(_.toEdgeDataWithUpdatedCost(rn)).map(_.dropRight(1).drop(1))
          agentHist              <- IO.fromEither(hist.getAgentHistoryOrError(request.agent))
          currentHist            <- IO.fromEither(agentHist.currentRequest)
          destinationLink        <- IO.fromOption(currentHist.route.lastOption)(new Error(s"agent route is empty"))
          destinationLinkUpdated <- destinationLink.updateCost(rn)
          remainingUpdated       <- currentHist.remainingRoute.traverse(_.updateCost(rn))
          experienced     = currentHist.experiencedRoute
          sharedRemaining = remainingUpdated.takeWhile(_.edgeId != forkEdgeId)
          oldCost         = costOfFullPath(experienced, sharedRemaining, oldEdgeData, destinationLinkUpdated)
          newCost         = costOfFullPath(experienced, sharedRemaining, newEdgeData, destinationLinkUpdated)
          offset          = if (oldCost == 0.0) 0.0 else (newCost - oldCost) / oldCost
          // _               = logger.info(s"shared path: ${EdgeData.mkString(experienced :++ sharedRemaining)}")
          // _               = logger.info(s"old: $oldCost ${EdgeData.mkString(oldEdgeData)}")
          // _               = logger.info(s"new: $newCost ${EdgeData.mkString(newEdgeData)}")
          // _               = logger.info(s"shared destination link: $destinationLinkUpdated")
          // _               = logger.info(s"final offset value: ($newCost - $oldCost) / $oldCost = $offset")
        } yield offset
      case other => IO.raiseError(new Error(s"expected 2 paths, found ${other.length}"))
    }
  }

  def costOfFullPath(
    experienced: List[EdgeData],
    middle: List[EdgeData],
    spur: List[EdgeData],
    dest: EdgeData
  ): Double = {
    val fullPath = experienced :++ middle :++ spur :+ dest
    fullPath.foldLeft(0.0)(_ + _.estimatedTimeAtEdge.map(_.value.toDouble).getOrElse(0.0))
  }

  def grabForkEdgeId(newPath: Path, oldPath: Path): IO[EdgeId] = {
    val matchResult = (newPath.headOption, oldPath.headOption).flatMapN {
      case (newEdge, oldEdge) =>
        if (newEdge.edgeId != oldEdge.edgeId) None else Some(newEdge.edgeId)
    }
    IO.fromOption(matchResult)(new Error(s"generatePathPair paths don't start at same Edge"))
  }

  /**
    * estimates the total trip travel time after replacing some later
    * segment of the trip with a path spur.
    *
    * ensures we use experienced travel times for the experienced part of
    * the route and use up-to-date estimates for any future segments.
    *
    * @param rn the road network state
    * @param history the history for this agent
    * @param pathSpur the path spur to apply
    * @return the effect of estimating the total travel time with this spur
    */
  def pathAlternativeTravelTimeEstimate(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    history: AgentHistory,
    pathSpur: Path
  ): IO[Double] =
    for {
      current   <- IO.fromEither(history.currentRequest)
      edgesSpur <- pathSpur.traverse { _.toEdgeDataWithUpdatedCost(rn) }
      remWithSpur = coalesceFuturePath(current.remainingRoute, edgesSpur)
      tts <- travelTime(rn, current.experiencedRoute, remWithSpur)
      // _  = logger.info(s"travel times: ${tts.map { t => f"$t%.2f".padTo(6, ' ') }}")
      tt = tts.foldLeft(0.0) { _ + _ }
    } yield tt

  /**
    * divides free flow travel time by the experienced + estimated travel time for a route
    * which is built from any experienced travel plus the estimated travel. the estimated
    * travel can be preempted by an optional new path spur that departs from the current
    * estimated route at some point in the future.
    *
    * a few concerns are managed here:
    *  - paths are PathSegments, but most code here expects EdgeData
    *  - free flow needs to be estimated over the complete
    *
    * @param rn
    * @param history
    * @param spurEdges if included, provides a path spur with origin that is present in the
    *                  current remaining trip and with destination that matches the destination
    *                  of the remaining trip
    * @return         ff / tt, which has it that 1.0 is NO delay, and 0.0 is close to infinite delays
    */
  def freeFlowOverTravelTimePercent(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    history: AgentHistory,
    spurEdges: List[EdgeData] = List.empty
  ): IO[Double] =
    for {
      // estimate travel time over experienced and remaining routes with optional route spur
      currentHist <- IO.fromEither(history.currentRequest)
      experiencedRoute = currentHist.experiencedRoute
      remWithSpur      = coalesceFuturePath(currentHist.remainingRoute, spurEdges)
      tt <- travelTime(rn, experiencedRoute, remWithSpur).map { _.foldLeft(0.0) { _ + _ } }
      // get the free flow travel time for the same route
      totalRoute = coalesceFuturePath(experiencedRoute, remWithSpur)
      fftt <- freeFlowTravelTime(rn, totalRoute).map { _.foldLeft(0.0) { _ + _ } }
      observation = if (tt == 0.0) 0.0 else fftt / tt
    } yield observation

  /**
    * takes a path and some future path spur and coalesces it into a single path.
    * steps through the current path until we find the first link of the spur and
    * attaches the spur at that point.
    *
    * @param path the path that we attach a spur to
    * @param spur the spur to append
    * @return a coalesced path from both inputs
    */
  def coalesceFuturePath(path: List[EdgeData], spur: List[EdgeData]): List[EdgeData] = {
    spur match {
      case Nil => path
      case spurOrigin :: _ =>
        path.takeWhile(_.edgeId != spurOrigin.edgeId) ::: spur
    }
  }

  // def coalesce

  /**
    * a path alternative may exit the remaining trip plan at some point in the future.
    *
    * this method traverses to find the jump-off point for a path alternative from the
    * current remaining route and then composes the stub for the current route with
    * that of the path alternative.
    *
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @param pathAlternative a branch from the remaining current route
    * @return cost estimate for the stub + the alt
    */
  def costOfRemainingWithAlternative(remainingCurrentRoute: List[EdgeData])(pathAlternative: Path): Double = {
    // all links in the alternative will be traversed
    val pathAltCost = pathAlternative.map { _.cost.value }.foldLeft(0.0) { _ + _ }

    // only the edges before the start of the path alternative will be traversed
    val remainingEdges =
      pathAlternative match {
        case Nil             => remainingCurrentRoute
        case spurOrigin :: _ => remainingCurrentRoute.takeWhile(_.edgeId != spurOrigin.edgeId)
      }
    val remainingCost = remainingEdges.flatMap { _.estimatedTimeAtEdge }.foldLeft(0.0) { _ + _.value.toDouble }

    remainingCost + pathAltCost
  }

  /**
    * the travel time for a route. this includes observed travel times
    * during the 'experienced' phase of the route and estimated travel
    * times for the 'remaining' phase.
    *
    * @param rn road network
    * @param experienced experienced route
    * @param remaining remaining route
    * @return effect of putting experienced + remaining travel time values
    * together in a list
    */
  def travelTime(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    experienced: List[EdgeData],
    remaining: List[EdgeData]
  ): IO[List[Double]] = {

    val expTT = experienced.flatMap(_.estimatedTimeAtEdge.map(_.value.toDouble))

    def _tt(ed: EdgeData) =
      for {
        eaOpt <- rn.edge(ed.edgeId)
        ea    <- IO.fromOption(eaOpt)(new Error(s"edge ${ed.edgeId} missing attr"))
      } yield ea.attribute.observedTravelTime.value

    for { remTT <- remaining.traverse(_tt) } yield expTT ::: remTT
  }

  /**
    * computes the free flow travel time for each edge
    *
    * @param rn road network state
    * @param route list of edges to get free flow travel time for
    * @return the free flow travel time of each edge in the route
    */
  def freeFlowTravelTime(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    route: List[EdgeData]
  ): IO[List[Double]] = {

    def _ff(ed: EdgeData) =
      for {
        eaOpt <- rn.edge(ed.edgeId)
        ea    <- IO.fromOption(eaOpt)(new Error(s"edge ${ed.edgeId} missing attr"))
      } yield ea.attribute.freeFlowTravelTime.value

    route.traverse(_ff)
  }

  def invertPercent(value: Double): Double = 1.0 - value

  def invertValue(min: Double, max: Double, value: Double): Double = {
    val pos    = (value - min) / (max - min)
    val invPos = 1.0 - pos
    val result = ((max - min) * invPos) + min
    result
  }
}
