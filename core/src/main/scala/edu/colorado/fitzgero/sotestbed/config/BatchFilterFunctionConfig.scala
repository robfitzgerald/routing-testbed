package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.{
  BatchFilterFunction,
  FilterByOverlapThreshold,
  FilterByTopKRanking
}

sealed trait BatchFilterFunctionConfig {

  def build(minBatchSearchSpace: Option[Int]): BatchFilterFunction
}

object BatchFilterFunctionConfig {

  /**
    * applies no filter to the incoming batches
    */
  final case object NoFilter extends BatchFilterFunctionConfig {

    def build(minBatchSearchSpace: Option[Int]): BatchFilterFunction = new BatchFilterFunction {
      def filter(batches: List[AltPathsAlgorithmResult]): List[AltPathsAlgorithmResult] = batches
    }
  }

  /**
    * only takes batches with overlap value above the given threshold, using
    * the provided batch overlap function
    * @param threshold the overlap percentage threshold that must be exceeded to avoid filtering
    * @param batchOverlapFunction computes the batch's overlap percentage
    */
  final case class OverlapThresholdBatchFilter(
    threshold: Double,
    batchOverlapFunction: BatchOverlapFunctionConfig
  ) extends BatchFilterFunctionConfig {

    def build(minBatchSearchSpace: Option[Int]): BatchFilterFunction =
      FilterByOverlapThreshold(threshold, minBatchSearchSpace, batchOverlapFunction.build())
  }

  /**
    * only takes the top k batches as sorted based on their overlap percentage as
    * computed with the provided batch overlap function
    * @param k the number of batches to accept
    * @param batchOverlapFunction computes the batch's overlap percentage
    */
  final case class TopKRankingBatchFilter(
    k: Int,
    batchOverlapFunction: BatchOverlapFunctionConfig
  ) extends BatchFilterFunctionConfig {

    def build(minBatchSearchSpace: Option[Int]): BatchFilterFunction =
      FilterByTopKRanking(k, minBatchSearchSpace, batchOverlapFunction.build())
  }
}
