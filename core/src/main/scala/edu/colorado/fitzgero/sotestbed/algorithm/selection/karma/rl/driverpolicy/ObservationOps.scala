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

object ObservationOps {

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
  def travelTimeDiffFromInitialTrip(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost
  )(
    history: ActiveAgentHistory.AgentHistory,
    proposedPaths: List[Path]
  ): IO[List[Double]] = {
    val result = for {
      o <- history.first.overallTravelTimeEstimate(rn, cf)
      c = history.current.experiencedTravelTime
      costs <- costPerProposedPath(proposedPaths, rn, cf)
    } yield {
      costs.map { tailCost => o.value - (c.value + tailCost) }
    }

    result
  }

  def costPerProposedPath(
    proposedPaths: List[Path],
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost
  ): IO[List[Double]] = {
    val costPerProposedPath = proposedPaths
      .traverse {
        _.traverse { e =>
          rn.edge(e.edgeId).map {
            case None     => None
            case Some(ea) => Some(cf(ea.attribute).value)
          }
        }
      }
      .map {
        _.map {
          _.flatten match {
            case Nil   => 0
            case costs => costs.sum
          }
        }
      }
    costPerProposedPath
  }
}
