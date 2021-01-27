package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

sealed trait PathOverlapLookupType

object PathOverlapLookupType {

  final case object TSP      extends PathOverlapLookupType
  final case object AllPaths extends PathOverlapLookupType

}
