package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.congestion

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{RoadNetwork, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

object CongestionOps {

  /**
    * computes a mapping from Grid Id to the normalized congestion effect
    * each row can range from 0.0% (free flow) to positive values measuring
    * the travel time increase from the free flow speed as:
    * (currentSpeed - freeFlow) / freeFlow
    * @param grid2 the grid we match the road network to
    * @param roadNetwork the current road network state
    * @param costFunction edge cost function
    * @return a mapping from Grid Cell Id to congestion measure
    */
  def congestionByCell(
    grid2: CoordinateGrid2,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  ): IO[Map[String, Double]] = {

    val result = for {
      edges    <- roadNetwork.edgeTriplets
      vertices <- roadNetwork.vertices
    } yield {
      val vertexLookup: Map[VertexId, Coordinate] = vertices.map { pair => pair.vertexId -> pair.attribute }.toMap

      // observe all normalized congestion values for each network edge and
      // store it by the GridId it is associated with
      // we want values for every grid cell even if it doesn't correspond with
      // any links, so, our initial accumulator has every grid id and empty collections for each
      val initial = grid2.gridCells.mapValues(_ => List.empty[Double])
      val observations =
        edges.foldLeft(initial) { (acc, edge) =>
          val inspectEdgeResult = for {
            src <- vertexLookup
              .get(edge.src)
              .toRight(new Error(s"edge ${edge.edgeId} has invalid src vertex ${edge.src}"))
            gridId <- grid2.getGridId(src.x, src.y)
          } yield {
            val freeflow         = Cost(edge.attr.freeFlowTravelTime.value)
            val current          = costFunction(edge.attr)
            val congestionEffect = ((current - freeflow) / freeflow).value
            val previousEntry    = acc.getOrElse(gridId, List.empty)
            acc.updated(gridId, congestionEffect +: previousEntry)
          }

          inspectEdgeResult.getOrElse(acc)
        }

      // average the congestion effect across all observations within each grid cell
      val normalizedCongestionByGridCell = for {
        (gridId, gridCellObservations) <- observations
      } yield {
        val avg =
          if (gridCellObservations.isEmpty) 0.0 else gridCellObservations.sum / gridCellObservations.length
        gridId -> avg
      }

      normalizedCongestionByGridCell
    }

    result
  }
}
