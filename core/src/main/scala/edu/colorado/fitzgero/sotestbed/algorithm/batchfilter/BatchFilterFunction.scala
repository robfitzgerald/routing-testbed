package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

trait BatchFilterFunction {

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches the batches with their (filtered) alts
    * @param roadNetwork the current road network state
    * @return the filtered result
    */
  def filter[F[_]: Monad, V, E](
    batches: List[AltPathsAlgorithmResult],
    roadNetwork: RoadNetwork[F, V, E]
  ): F[List[AltPathsAlgorithmResult]]
}
