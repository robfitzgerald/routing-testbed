package edu.colorado.fitzgero.sotestbed.config

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.{
  BatchFilterFunction,
  FilterAllAgentsByOverlapThreshold,
  FilterAllAgentsByTopKOverlap,
  FilterByOverlapThreshold,
  FilterByTopKCongestion,
  FilterByTopKOverlapAndCongestion,
  FilterByTopKRanking
}
import edu.colorado.fitzgero.sotestbed.algorithm.grid.CoordinateGrid2
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait BatchFilterFunctionConfig {

  def build(
    minBatchSearchSpace: Option[Int],
    grid: CoordinateGrid2,
    costFunction: EdgeBPR => Cost
  ): BatchFilterFunction
}

object BatchFilterFunctionConfig {

  /**
    * applies no filter to the incoming batches
    */
  final case object NoFilter extends BatchFilterFunctionConfig {

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction = new BatchFilterFunction {

      def filter(
        batches: List[AltPathsAlgorithmResult],
        roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
      ): IO[List[AltPathsAlgorithmResult]] = IO(batches)
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

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction =
      FilterByOverlapThreshold(
        threshold,
        minBatchSearchSpace,
        batchOverlapFunction.build(),
        batchOverlapFunction.overlapCostType
      )
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

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction =
      FilterByTopKRanking(
        k,
        minBatchSearchSpace,
        batchOverlapFunction.build(),
        batchOverlapFunction.overlapCostType
      )
  }

  /**
    * only takes the top k batches as ranked based on the congestion observed in the
    * grid cell associated with the batch's batch id
    *
    * @param k top-k batches
    */
  final case class TopKCongestionBatchFilter(k: Int) extends BatchFilterFunctionConfig {

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction =
      FilterByTopKCongestion(k, minBatchSearchSpace, grid, costFunction)
  }

  /**
    * batch filter that prioritizes batches with higher overlap and higher
    * congestion (travel time increase %).
    *
    * @param k top-k batches
    * @param batchOverlapFunction way to compute overlap percentage
    */
  final case class TopKOverlapAndCongestionRankingBatchFilter(
    k: Int,
    batchOverlapFunction: BatchOverlapFunctionConfig
  ) extends BatchFilterFunctionConfig {

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction =
      FilterByTopKOverlapAndCongestion(
        k,
        minBatchSearchSpace,
        batchOverlapFunction.build(),
        batchOverlapFunction.overlapCostType,
        grid,
        costFunction
      )
  }

  /**
    * with batch grouping deactivated, this filter will look at all agents under control and
    * remove agents from the batch whose overlap is less than some threshold.
    *
    * @param threshold the overlap percentage threshold that must be exceeded to avoid filtering
    * @param batchOverlapFunction computes the batch's overlap percentage
    */
  final case class FilterAllAgentsByOverlapThresholdFilter(
    threshold: Double,
    batchOverlapFunction: BatchOverlapFunctionConfig
  ) extends BatchFilterFunctionConfig {

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction = {
      FilterAllAgentsByOverlapThreshold(threshold, batchOverlapFunction.build())
    }
  }

  /**
    * with batch grouping deactivated, this filter will look at all agents under control and
    * remove agents from the batch whose overlap is less than some threshold.
    *
    * @param k top-k batches
    * @param batchOverlapFunction computes the batch's overlap percentage
    */
  final case class FilterAllAgentsByTopKOverlapFilter(
    k: Int,
    batchOverlapFunction: BatchOverlapFunctionConfig
  ) extends BatchFilterFunctionConfig {

    def build(
      minBatchSearchSpace: Option[Int],
      grid: CoordinateGrid2,
      costFunction: EdgeBPR => Cost
    ): BatchFilterFunction = {
      FilterAllAgentsByTopKOverlap(k, batchOverlapFunction.build())
    }
  }
}
