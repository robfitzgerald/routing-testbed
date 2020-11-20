package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult

trait BatchFilterFunction {

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches the batches with their (filtered) alts
    * @return the filtered result
    */
  def filter(batches: List[AltPathsAlgorithmResult]): List[AltPathsAlgorithmResult]
}
