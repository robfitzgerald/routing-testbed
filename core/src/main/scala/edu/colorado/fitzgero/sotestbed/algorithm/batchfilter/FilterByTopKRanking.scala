package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.BatchOverlapFunction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm

final case class FilterByTopKRanking(
  k: Int,
  minBatchSearchSpace: Option[Int],
  batchOverlapFunction: BatchOverlapFunction
) extends BatchFilterFunction
    with LazyLogging {

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches the batches with their (filtered) alts
    * @return the filtered result
    */
  def filter(batches: List[AltPathsAlgorithmResult]): List[AltPathsAlgorithmResult] = {
    val ranked: List[(Double, AltPathsAlgorithmResult)] = for {
      batch <- batches
      alts          = batch.filteredAlts.getOrElse(batch.alts)
      overlapResult = batchOverlapFunction(alts)
      meetsMinSearchSpaceSizeConstraint = minBatchSearchSpace
        .map(t => SelectionAlgorithm.numCombinationsGreaterThanThreshold(alts, t))
        .getOrElse(true)
      if meetsMinSearchSpaceSizeConstraint
    } yield (overlapResult.average, batch)

    val filtered = ranked.sortBy { case (rank, _) => -rank }.take(k)

    logger.whenInfoEnabled {
      val inRanks = ranked
        .sortBy { case (_, b) => b.batchId }
        .map { case (r, b) => f"(id=${b.batchId}:k=${b.alts.size}:${r * 100.0}%.1f%%)" }
        .mkString("{", ", ", "}")
      val inCount   = batches.length
      val rankCount = ranked.length
      val outCount  = filtered.length
      val ranksMsg = filtered
        .map { case (r, b) => f"(id=${b.batchId}:k=${b.alts.size}:${r * 100.0}%.1f%%)" }
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
