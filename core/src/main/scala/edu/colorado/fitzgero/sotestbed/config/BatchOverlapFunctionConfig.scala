package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.{
  AgentProportionalBatchOverlap,
  BatchOverlapFunction,
  BatchProportionalOverlap,
  OverlapCostType,
  PathOverlapLookup,
  PathOverlapLookupOps,
  PathOverlapLookupType
}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

sealed trait BatchOverlapFunctionConfig {
  def build(): BatchOverlapFunction
  def overlapCostType: OverlapCostType
}

object BatchOverlapFunctionConfig {

  final case class AgentProportional(
    pathOverlapLookupType: PathOverlapLookupType,
    overlapCostType: OverlapCostType,
    useLinkWeights: Boolean
  ) extends BatchOverlapFunctionConfig {

    override def build(): BatchOverlapFunction = {
      val pathOverlapLookup: PathOverlapLookup = pathOverlapLookupType match {
        case PathOverlapLookupType.TSP =>
          PathOverlapLookupOps.trueShortestPathLookup(useLinkWeights)
        case PathOverlapLookupType.AllPaths =>
          PathOverlapLookupOps.altPathsLookup(useLinkWeights)
      }
      (alts: Map[Request, List[Path]]) => AgentProportionalBatchOverlap.from(alts, pathOverlapLookup, overlapCostType)
    }
  }

  final case class BatchProportional(
    pathOverlapLookupType: PathOverlapLookupType,
    overlapCostType: OverlapCostType,
    useLinkWeights: Boolean
  ) extends BatchOverlapFunctionConfig {

    override def build(): BatchOverlapFunction = {
      val pathOverlapLookup: PathOverlapLookup = pathOverlapLookupType match {
        case PathOverlapLookupType.TSP =>
          PathOverlapLookupOps.trueShortestPathLookup(useLinkWeights)
        case PathOverlapLookupType.AllPaths =>
          PathOverlapLookupOps.altPathsLookup(useLinkWeights)
      }
      (alts: Map[Request, List[Path]]) => BatchProportionalOverlap.from(alts, pathOverlapLookup, overlapCostType)
    }
  }
}
