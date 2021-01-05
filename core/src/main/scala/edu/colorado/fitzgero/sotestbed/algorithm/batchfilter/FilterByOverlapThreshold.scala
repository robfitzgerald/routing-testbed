package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.Monad

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.BatchOverlapFunction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

final case class FilterByOverlapThreshold(
  threshold: Double,
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
  def filter[F[_]: Monad, V, E](
    batches: List[AltPathsAlgorithmResult],
    roadNetwork: RoadNetwork[F, V, E]
  ): F[List[AltPathsAlgorithmResult]] = {
    val result = for {
      batch <- batches
      alts          = batch.filteredAlts.getOrElse(batch.alts)
      overlapResult = batchOverlapFunction(alts)
      meetsMinSearchSpaceSizeConstraint = minBatchSearchSpace
        .map(t => SelectionAlgorithm.numCombinationsGreaterThanThreshold(alts, t))
        .getOrElse(true)
      if meetsMinSearchSpaceSizeConstraint && overlapResult.average >= this.threshold
    } yield batch

    logger.whenInfoEnabled {
      val inCount  = batches.length
      val outCount = result.length
      val msssMsg  = minBatchSearchSpace.map(n => s" and min search space size $n").getOrElse("")
      logger.info(
        f"filtered $inCount batches down to $outCount due to overlap threshold of ${this.threshold * 100}%.2f%%$msssMsg"
      )
    }

    Monad[F].pure(result)
  }
}
