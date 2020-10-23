package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

trait BatchFilterFunction {

  /**
    * takes all batches of requests with computed alternate paths, and possibly
    * removes some batches based on a batch filtering model
    *
    * @param batches the batches with their (filtered) alts
    * @return the filtered result
    */
  def filter(batches: List[Map[Request, List[Path]]]): List[Map[Request, List[Path]]]
}
