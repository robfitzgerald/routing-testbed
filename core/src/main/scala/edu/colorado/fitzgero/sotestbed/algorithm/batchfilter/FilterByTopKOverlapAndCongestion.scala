package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.BatchOverlapFunction
import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{RoadNetwork, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

final case class FilterByTopKOverlapAndCongestion(
  k: Int,
  minBatchSearchSpace: Option[Int],
  batchOverlapFunction: BatchOverlapFunction,
  grid: CoordinateGrid2,
  costFunction: EdgeBPR => Cost
) extends BatchFilterFunction
    with LazyLogging {

  import FilterByTopKOverlapAndCongestion._

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches the batches with their (filtered) alts
    * @return the filtered result
    */
  def filter(
    batches: List[AltPathsAlgorithmResult],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[List[AltPathsAlgorithmResult]] = {

    val result = for {
      congestionLookup <- FilterByTopKOverlapAndCongestion.congestionByCell(
        grid,
        roadNetwork,
        costFunction
      )
    } yield {

      val ranked: List[RankedAlternative] = for {
        batch <- batches
        alts          = batch.filteredAlts.getOrElse(batch.alts)
        overlapResult = batchOverlapFunction(alts)
        congestion <- congestionLookup.get(batch.batchId)
        meetsMinSearchSpaceSizeConstraint = minBatchSearchSpace
          .map(t => SelectionAlgorithm.numCombinationsGreaterThanThreshold(alts, t))
          .getOrElse(true)
        if meetsMinSearchSpaceSizeConstraint
      } yield {
        val ranking: Double = overlapResult.average * congestion
        RankedAlternative(ranking, overlapResult.average, congestion, batch)
      }

      val filtered = ranked.sortBy { case RankedAlternative(rank, _, _, _) => -rank }.take(k)

      logger.whenInfoEnabled {
        val inRanks = ranked
          .sortBy { case RankedAlternative(_, _, _, b) => b.batchId }
          .map {
            case RankedAlternative(r, o, c, b) =>
              val rank       = f"${r * 100.0}%.1f%%"
              val overlap    = f"${o * 100.0}%.1f%%"
              val congestion = f"${c * 100.0}%.1f%%"
              f"(id=${b.batchId}:k=${b.alts.size}:rank=$rank:overlap=$overlap:congestion=$congestion)"
          }
          .mkString("{", ", ", "}")
        val inCount   = batches.length
        val rankCount = ranked.length
        val outCount  = filtered.length
        val selectedRanks = filtered
          .map {
            case RankedAlternative(r, o, c, b) =>
              val rank       = f"${r * 100.0}%.1f%%"
              val overlap    = f"${o * 100.0}%.1f%%"
              val congestion = f"${c * 100.0}%.1f%%"
              f"(id=${b.batchId}:k=${b.alts.size}:rank=$rank:overlap=$overlap:congestion=$congestion)"
          }
          .mkString("{", ", ", "}")
        val msssMsg2 =
          minBatchSearchSpace.map(n => s" down to $rankCount due to search space size, and finally").getOrElse("")
        logger.info(s"input batches with overlap percentage: $inRanks")
        logger.info(s"filtered $inCount batches$msssMsg2 down to $outCount due to top-$k ranking:\n$selectedRanks")
      }

      filtered.map { _.altPathsAlgorithmResult }

    }

    result
  }
}

object FilterByTopKOverlapAndCongestion {

  final case class RankedAlternative(
    rank: Double,
    overlapPercent: Double,
    congestionIncreasePercent: Double,
    altPathsAlgorithmResult: AltPathsAlgorithmResult
  )

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
            val congestionEffect = (current - freeflow / freeflow).value
            val previousEntry    = acc.getOrElse(gridId, List.empty)
            acc.updated(gridId, congestionEffect +: previousEntry)
          }

          inspectEdgeResult.getOrElse(acc)
        }

      // average the congestion effect across all observations within each grid cell
      val normalizedCongestionByGridCell = for {
        (gridId, gridCellObservations) <- observations
      } yield {
        val avg = if (gridCellObservations.isEmpty) 0.0 else gridCellObservations.sum / gridCellObservations.length
        gridId -> avg
      }

      normalizedCongestionByGridCell
    }

    result
  }
}
