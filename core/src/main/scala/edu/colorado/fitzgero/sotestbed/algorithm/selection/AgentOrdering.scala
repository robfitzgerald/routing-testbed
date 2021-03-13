package edu.colorado.fitzgero.sotestbed.algorithm.selection

import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.{
  BatchProportionalOverlap,
  OverlapCostType,
  PathOverlapLookupOps,
  PathOverlapLookupType
}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

sealed trait AgentOrdering

object AgentOrdering {

  case class BatchProportional(ascending: Option[Boolean]) extends AgentOrdering {

    def ordering(alts: Map[Request, List[Path]]): Ordering[(Request, List[Path])] = {
      val batchOverlapResult = BatchProportionalOverlap.from(
        alts,
        PathOverlapLookupOps.altPathsLookup(),
        OverlapCostType.NonNormalized
      )
      Ordering.by {
        case (req, _) =>
          val overlapValue = batchOverlapResult.overlapValues.getOrElse(req, 0.0)
          val rank         = if (ascending.exists { identity }) overlapValue else -overlapValue
          rank
      }
    }
  }

}
