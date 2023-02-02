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

object DriverPolicySpaceV2Ops {

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

  def pathAlternativeTravelTimeEstimate(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    history: AgentHistory,
    pathSpur: Path
  ): IO[Double] =
    for {
      current   <- IO.fromEither(history.currentRequest)
      edgesSpur <- pathSpur.traverse { _.toEdgeData(rn) }
      remWithSpur = coalesceFuturePath(current.remainingRoute, edgesSpur)
      tt <- travelTime(rn, current.experiencedRoute, remWithSpur).map { _.foldLeft(0.0) { _ + _ } }
      // remainingCost = costOfRemainingWithAlternative(current.remainingRoute)(path)
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
      currentHist <- IO.fromEither(history.currentRequest)
      experiencedRoute = currentHist.experiencedRoute
      remWithSpur      = coalesceFuturePath(currentHist.remainingRoute, spurEdges)
      totalRoute       = coalesceFuturePath(experiencedRoute, remWithSpur)
      // estimate travel time over experienced and remaining routes with optional route spur
      tt <- travelTime(rn, experiencedRoute, remWithSpur).map { _.foldLeft(0.0) { _ + _ } }
      // travelTime = totalRoute.flatMap { _.estimatedTimeAtEdge }.foldLeft(0.0) { _ + _.value }
      // get the free flow travel time for the same route
      fftt <- freeFlowTravelTime(rn, totalRoute).map { _.foldLeft(0.0) { _ + _ } }
      observation  = if (tt == 0.0) 0.0 else fftt / tt
      obsTruncated = math.max(0.0, math.min(1.0, observation))
    } yield obsTruncated

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

    for {
      remTT <- remaining.traverse(_tt)
    } yield expTT ::: remTT
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
}
