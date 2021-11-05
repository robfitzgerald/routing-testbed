package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

trait BatchFilterResult {
  def alts: Map[Request, List[Path]]
  def rank: Double
}
