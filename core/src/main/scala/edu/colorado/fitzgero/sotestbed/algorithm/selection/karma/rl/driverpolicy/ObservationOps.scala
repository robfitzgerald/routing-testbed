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
          o <- IO.fromEither(history.originalRequest.overallTravelTimeEstimate)
          c <- IO.fromEither(history.currentRequest.overallTravelTimeEstimate)
          diff = o.value - c.value
        } yield diff
        result
    }
  }

  // /**
  //   * for the remaining trip, consider a set of alternatives, and compute the
  //   * remaining distance for each, including any of the current path which
  //   * must be traversed in order to reach the beginning of that alternative.
  //   *
  //   * @param history history for the agent associated with this set
  //   * of proposed paths
  //   * @param proposedPaths paths proposed by the alt paths algorithm
  //   * @return for each path (by index), the distance for that alternative
  //   */
  // def distanceOfAlternatives(
  //   history: AgentHistory,
  //   proposedPaths: List[Path]
  // ): List[Double] = {
  //   val currentRemainingPath = history.currentRequest.remainingRoute
  //   proposedPaths.map { alt =>
  //     val thisRemainingTime = coalesceCostFor(currentRemainingPath, alt)
  //     thisRemainingTime
  //   }
  // }

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
