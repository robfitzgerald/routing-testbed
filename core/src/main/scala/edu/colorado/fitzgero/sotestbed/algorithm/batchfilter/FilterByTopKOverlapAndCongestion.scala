package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.{BatchOverlapFunction, OverlapCostType}
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.congestion.CongestionOps
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
  overlapCostType: OverlapCostType,
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
      congestionLookup <- CongestionOps.congestionByCell(
        grid,
        roadNetwork,
        costFunction
      )
    } yield {

      val ranked: List[RankedAlternative] = for {
        batch <- batches
        alts          = batch.filteredAlts.getOrElse(batch.alts)
        overlapResult = batchOverlapFunction(alts)
        congestion <- congestionLookup.get(batch.batchId) match {
          case Some(value) => Some(value)
          case None =>
            logger.error(
              s"congestion filtering only implemented for batch ids that map to one cell, but got ${batch.batchId}"
            )
            None
        }
        meetsMinSearchSpaceSizeConstraint = minBatchSearchSpace
          .map(t => SelectionAlgorithm.numCombinationsGreaterThanThreshold(alts, t))
          .getOrElse(true)
        if meetsMinSearchSpaceSizeConstraint
      } yield {
        val overlapCost     = overlapCostType.aggregate(overlapResult.overlapValues.values)
        val ranking: Double = overlapCost * congestion
        RankedAlternative(ranking, overlapCost, congestion, batch)
      }

      val filtered = ranked.sortBy { case RankedAlternative(rank, _, _, _) => -rank }.take(k)

      logger.whenInfoEnabled {
        val inRanks = ranked
          .sortBy { case RankedAlternative(_, _, _, b) => b.batchId }
          .map {
            case RankedAlternative(r, o, c, b) =>
              val rank       = f"${r * 100.0}%.1f%%"
              val overlap    = overlapCostType.print(o)
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
              val overlap    = overlapCostType.print(o)
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

}
