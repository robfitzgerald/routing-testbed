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

  def pathAlternativeTravelTimeEstimate(history: AgentHistory, path: Path): IO[Double] =
    for {
      current <- IO.fromEither(history.currentRequest)
      remainingCost = costOfRemainingWithAlternative(current.remainingRoute)(path)
    } yield current.experiencedTravelTime.value + remainingCost

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
    * @param path
    * @return
    */
  def freeFlowOverTravelTimePercent(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    history: AgentHistory,
    spurEdges: List[EdgeData]
  ): IO[Double] =
    for {
      currentHist <- IO.fromEither(history.currentRequest)
      // build the proposed (complete) route
      experiencedRoute = currentHist.experiencedRoute
      remWithSpur      = coalesceFuturePath(currentHist.remainingRoute, spurEdges)
      totalRoute       = coalesceFuturePath(experiencedRoute, remWithSpur)
      // estimate travel time and free flow travel time of this (total) route
      travelTime = totalRoute.map { _.estimatedTimeAtEdge }.flatten.foldLeft(0.0) { _ + _.value }
      fftt <- freeFlowTravelTime(rn, totalRoute).map { _.foldLeft(0.0) { _ + _ } }
      observation  = if (fftt == 0.0) 0.0 else travelTime / fftt
      obsTruncated = math.max(0.0, math.min(1.0, observation))
    } yield obsTruncated

  def coalesceFuturePath(remaining: List[EdgeData], alternative: List[EdgeData]): List[EdgeData] = {
    alternative match {
      case Nil => remaining
      case spurOrigin :: _ =>
        remaining.takeWhile(_.edgeId != spurOrigin.edgeId) ::: alternative
    }
  }

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
    * find the diffs by edge between observed
    * travel time and the free flow travel time, and sums them.
    *
    * @param rn road network state
    * @param route list of edges we have traversed with their travel time
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
