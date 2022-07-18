package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma

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
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost,
    history: ActiveAgentHistory.AgentHistory
  ): IO[Double] = {
    history.history match {
      case Nil => IO.pure(0.0)
      case latest :: tail =>
        val result = for {
          o <- history.first.overallTravelTimeEstimate(rn, cf)
          c = history.current.experiencedTravelTime
          p = history.current.remainingRoute.map { _.toPathSegment }
          remainingCost <- currentPathCost(p, rn, cf)
          diff = o.value - (c.value + remainingCost)
        } yield diff
        result
    }
  }

  /**
    *
    *
    * @param rn the current road network state
    * @param cf the cost function to use when estimating travel times
    * @param history history for the agent associated with this set
    * of proposed paths
    * @param proposedPaths paths proposed by the alt paths algorithm
    * @return for each path (by index), the diff from the original path.
    * computed as (originalEstimate - (experiencedTime + proposedAddedTime))
    */
  def travelTimeDiffFromAlternatives(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost,
    history: ActiveAgentHistory.AgentHistory,
    proposedPaths: List[Path]
  ): IO[List[Double]] = {
    val result = for {
      o <- history.first.overallTravelTimeEstimate(rn, cf)
      c = history.current.experiencedTravelTime
      costs <- proposedPaths.traverse(p => currentPathCost(p, rn, cf))
    } yield {
      costs.map { tailCost => o.value - (c.value + tailCost) }
    }

    result
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
