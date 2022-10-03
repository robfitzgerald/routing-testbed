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

object ObservationOps {

  /**
    * finds the overall travel time difference in route estimates since this agent entered the system.
    * combines travel times observed with travel time estimates in order to estimate the complete route
    * travel time.
    *
    * @param rn the current road network state
    * @param cf the cost function to use when estimating travel times
    * @param history history for the agent, which will have at least one entry
    * @return either the difference in travel time estimate from the original
    * route to the current route. if only one route exists (agent just entered
    * the system), then zero is returned.
    */
  def travelTimeDiffFromInitialTrip(
    history: AgentHistory
  ): IO[Long] = {
    history.history match {
      case Nil => IO.pure(0L)
      case latest :: tail =>
        val result = for {
          oTT <- IO.fromEither(history.first.tripTravelTimeEstimate)
          c   <- IO.fromEither(history.currentRequest)
          cTT <- IO.fromEither(c.overallTravelTimeEstimate)
          diff = oTT.value - cTT.value
        } yield diff
        result
    }
  }

  /**
    * given an experienced route and a spur made from coalescing the remaining
    * route with an alt path, find the diffs by edge between observed/estimated
    * travel time and the free flow travel time.
    *
    * @param rn road network state
    * @param experiencedRoute list of edges we have traversed with their travel time
    * @param remainingRoute list of edges remaining on the current route plan (including current edge)
    *                       along with travel time estimates
    * @param altPath a spur that starts along our remaining route but has the same destination
    * @return the diff from free flow for this coalesced route with spur
    */
  def compareRouteToFreeFlow(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    experiencedRoute: List[EdgeData],
    remainingRoute: List[EdgeData]
  )(
    altPath: Path
  ): IO[Double] = {
    // get experienced route (EdgeData), remaining route (EdgeData), spur route (PathSegment)
    // build complete alt path from start of trip via spur to destination
    // for each edge, lookup free flow speed, compute diff
    val experiencedIO = experiencedRoute.traverse { e =>
      IO.fromOption(e.estimatedTimeAtEdge)(new Error(s"edge ${e.edgeId} missing time estimate"))
        .map { est => (e.edgeId, est.value.toDouble) }
    }
    val remainingWithSpurIO = timeCostByEdgeOfRootAndSpur(rn, remainingRoute)(altPath)

    val diffIO = for {
      experienced <- experiencedIO
      remaining   <- remainingWithSpurIO
      diffs <- (experienced ::: remaining).traverse {
        case (edgeId, obsTime) =>
          rn.edge(edgeId).flatMap { eaOpt =>
            IO.fromOption(eaOpt)(new Error(s"edge $edgeId missing attribute"))
              .map { ea => obsTime - ea.attribute.freeFlowTravelTime.value }
          }
      }
    } yield diffs.sum

    diffIO
  }

  /**
    * traverses to find the jump-off point for a path alternative spur from the current remaining route
    * and then composes the spur with the current route. computes the time estimate for the coalesced
    * path.
    *
    * @param rn current road network state
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @param path a branch from the remaining current route
    * @return function to compute distance estimate for the stub + the alt
    */
  def distanceCostOfRootAndSpur(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    remainingCurrentRoute: List[EdgeData]
  ): Path => IO[Double] = {
    val fn: Path => IO[(List[(EdgeId, Double)], Double)] =
      coalesceRootPathWithSpurPath(
        rn,
        e => e.distance.value,
        e => IO.pure(e.linkDistance),
        remainingCurrentRoute
      )
    (p: Path) => fn(p).map { case (_, cost) => cost }
  }

  /**
    * traverses to find the jump-off point for a path alternative spur from the current remaining route
    * and then composes the spur with the current route. computes the time estimate for the coalesced
    * path.
    *
    * @param rn current road network state
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @return function to compute time estimate for the stub + the alt
    */
  def timeCostOfRootAndSpur(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    remainingCurrentRoute: List[EdgeData]
  ): Path => IO[Double] = {
    val fn: Path => IO[(List[(EdgeId, Double)], Double)] =
      coalesceRootPathWithSpurPath(
        rn,
        e => e.observedTravelTime.value,
        e => IO.fromOption(e.estimatedTimeAtEdge.map { _.value.toDouble })(new Error(s"edge missing time est $e")),
        remainingCurrentRoute
      )
    (p: Path) => fn(p).map { case (_, cost) => cost }
  }

  /**
    * traverses to find the jump-off point for a path alternative spur from the current remaining route
    * and then composes the spur with the current route. computes the time estimate for the coalesced
    * path.
    *
    * @param rn current road network state
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @return function to compute time estimate for the stub + the alt
    */
  def timeCostByEdgeOfRootAndSpur(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    remainingCurrentRoute: List[EdgeData]
  ): Path => IO[List[(EdgeId, Double)]] = {
    val fn: Path => IO[(List[(EdgeId, Double)], Double)] =
      coalesceRootPathWithSpurPath(
        rn,
        e => e.observedTravelTime.value,
        e => IO.fromOption(e.estimatedTimeAtEdge.map { _.value.toDouble })(new Error(s"edge missing time est $e")),
        remainingCurrentRoute
      )
    (p: Path) => fn(p).map { case (path, _) => path }
  }

  /**
    * traverses to find the jump-off point for a path alternative spur from the current remaining route
    * and then composes the spur with the current route. we use two cost functions defined over
    * the unfortunately different data structures for the current path and the path spur.
    *
    * @param rn current road network state
    * @param edgeBPRFn function to extract a value from an EdgeBPR instance
    * @param edgeDataFn function to extract a value from an EdgeData instance
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @param path a branch from the remaining current route
    * @return cost estimate for the stub + the alt
    */
  def coalesceRootPathWithSpurPath(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    edgeBPRFn: EdgeBPR => Double,
    edgeDataFn: EdgeData => IO[Double],
    remainingCurrentRoute: List[EdgeData]
  )(path: Path): IO[(List[(EdgeId, Double)], Double)] = {
    path match {
      case Nil                                     => IO.pure((List.empty, 0.0))
      case PathSegment(pathAltStartEdgeId, _) :: _ =>
        // compute the value associated with this path alternative
        val pathWithCostsIO = path.traverse { seg =>
          rn.edge(seg.edgeId)
            .flatMap { opt =>
              IO.fromOption(opt)(new Error(s"edge has no attribute"))
                .map { ea => (seg.edgeId, edgeBPRFn(ea.attribute)) }
            }
        }

        remainingCurrentRoute.dropWhile(_.edgeId != pathAltStartEdgeId) match {
          case Nil =>
            // never encounter the starting edge of this path alternative along
            // our current route, so coalescing isn't well-defined here
            val currentPath = remainingCurrentRoute.map { e => s"-[${e.edgeId}]->" }.mkString(" ")
            val altPath     = path.map { e => s"-[${e.edgeId}]->" }.mkString(" ")
            val msg         = s"""attempting to coalesce two paths, one the current path, the
                         |second a spur. but the start edge of the spur path $pathAltStartEdgeId never appears
                         |in the first path. 
                         |current path: $currentPath
                         |spur path:    $altPath
                         |""".stripMargin
            IO.raiseError(new Error(msg))
          case spur =>
            val spurWithCosts = spur.traverse(edgeDataFn)
            for {
              pathWithCosts <- pathWithCostsIO
              spurWithCosts <- spur.traverse(e => edgeDataFn(e).map { c => (e.edgeId, c) })
              pathWithSpur = pathWithCosts ::: spurWithCosts
              cost         = pathWithSpur.foldLeft(0.0) { case (acc, (_, c)) => acc + c }
            } yield (pathWithSpur, cost)
        }
    }
  }

  /**
    * traverses to find the jump-off point for a path alternative from the current remaining route
    * and then composes the stub for the current route with that of the path alternative
    *
    * @param remainingCurrentRoute a route we may still travel on for a bit
    * @param pathAlternative a branch from the remaining current route
    * @return cost estimate for the stub + the alt
    */
  def coalesceCostFor(remainingCurrentRoute: List[EdgeData])(pathAlternative: Path): Double = {
    if (pathAlternative.isEmpty) 0.0
    else {
      val pathAltCost = pathAlternative.map { _.cost.value }.foldLeft(0.0) { _ + _ }
      remainingCurrentRoute.dropWhile(_.edgeId != pathAlternative.head.edgeId) match {
        case Nil                                           => pathAltCost
        case detached if detached == remainingCurrentRoute =>
          // if it used the whole current route, we would reach our destination...
          // so, these paths aren't connected, but let's at least include the alt's cost
          pathAltCost
        case stub =>
          val stubCost = stub.flatMap { _.estimatedTimeAtEdge }.foldLeft(0.0) { _ + _.value.toDouble }
          stubCost + pathAltCost
      }
    }
  }

  def currentPathCost(
    path: Path,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost
  ): IO[Double] = {
    val initial: IO[Double] = IO.pure(0.0)
    path.foldLeft(initial) { (acc, e) =>
      for {
        accumulator <- acc
        eaOpt       <- rn.edge(e.edgeId)
        ea          <- IO.fromOption(eaOpt)(new Error("missing edge"))
        c = cf(ea.attribute).value
      } yield accumulator + c
    }
  }
}
