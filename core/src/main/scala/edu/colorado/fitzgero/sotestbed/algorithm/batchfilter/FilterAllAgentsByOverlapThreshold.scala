package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.BatchOverlapFunction
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

case class FilterAllAgentsByOverlapThreshold(
  threshold: Double,
  batchOverlapFunction: BatchOverlapFunction
) extends BatchFilterFunction
    with LazyLogging {

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches     the batches with their (filtered) alts
    * @param roadNetwork the current road network state
    * @return the filtered result
    */
  def filter(
    batches: List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] = {
    // there should only be one batch at most
    val moreThanOneBatch: Boolean = batches.lengthCompare(1) > 0
    if (moreThanOneBatch) {
      val msg =
        "this filter only works when BatchGrouping is Greedy with no maxBatchSize; " +
          s"num batches should not exceed 1 but was ${batches.length}"
      IO.raiseError(new Error(msg))
    } else {

      // awkward, we need to mess with the innards of this AltPathsAlgorithmResult
      val overlapValuesOption = for {
        batch <- batches.headOption
        alts          = batch.filteredAlts.getOrElse(batch.alts)
        overlapResult = batchOverlapFunction(alts)
      } yield {
        alts.foldLeft(batch) {
          case (b, (req, _)) =>
            val rankGTEThreshold: Boolean = overlapResult.overlapValues
              .get(req)
              .exists { rank => rank >= this.threshold }

            if (rankGTEThreshold) {
              // keep this request
              b
            } else {
              // remove this request
              b.copy(
                alts = b.alts - req,
                filteredAlts = b.filteredAlts.map { fAlts => fAlts - req }
              )
            }
        }
      }

      val result: IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] =
        overlapValuesOption match {
          case None =>
            IO.pure(List.empty)
          case Some(updatedAltPathResult) =>
            logger.info(s"filtered replanning to ${updatedAltPathResult.alts.size}")
            IO.pure(List(updatedAltPathResult))
        }

      result
    }
  }
}
