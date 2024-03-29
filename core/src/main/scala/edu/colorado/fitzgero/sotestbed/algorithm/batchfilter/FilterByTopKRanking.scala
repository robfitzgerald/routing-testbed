package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.Monad
import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.{BatchOverlapFunction, OverlapCostType}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

final case class FilterByTopKRanking(
  k: Int,
  minBatchSearchSpace: Option[Int],
  batchOverlapFunction: BatchOverlapFunction,
  overlapCostType: OverlapCostType
) extends BatchFilterFunction
    with LazyLogging {

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
  ): IO[List[AltPathsAlgorithmResult]] = IO {
    val ranked: List[(Double, AltPathsAlgorithmResult)] = for {
      batch <- batches
      alts          = batch.filteredAlts.getOrElse(batch.alts)
      overlapResult = batchOverlapFunction(alts)
      meetsMinSearchSpaceSizeConstraint = minBatchSearchSpace
        .map(t => SelectionAlgorithm.numCombinationsGreaterThanThreshold(alts, t))
        .getOrElse(true)
      if meetsMinSearchSpaceSizeConstraint
    } yield (overlapCostType.aggregate(overlapResult.overlapValues.values), batch)

    val filtered = ranked.sortBy { case (rank, _) => -rank }.take(k)

    logger.whenInfoEnabled {
      val inRanks = ranked
        .sortBy { case (_, b) => b.batchId }
        .map { case (r, b) => f"(id=${b.batchId}:k=${b.alts.size}:${overlapCostType.print(r)})" }
        .mkString("{", ", ", "}")
      val inCount   = batches.length
      val rankCount = ranked.length
      val outCount  = filtered.length
      val ranksMsg = filtered
        .map { case (r, b) => f"(id=${b.batchId}:k=${b.alts.size}:o=${overlapCostType.print(r)})" }
        .mkString("{", ", ", "}")
      val msssMsg2 =
        minBatchSearchSpace.map(n => s" down to $rankCount due to search space size, and finally").getOrElse("")
      logger.info(s"input batches with overlap percentage: $inRanks")
      logger.info(s"filtered $inCount batches$msssMsg2 down to $outCount due to top-$k ranking:\n$ranksMsg")
    }

    val result = filtered.map { _._2 }

    result
  }
}
