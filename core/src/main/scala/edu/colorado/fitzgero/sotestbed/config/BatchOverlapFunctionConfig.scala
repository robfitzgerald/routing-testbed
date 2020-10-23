package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap.{
  AgentProportionalBatchOverlap,
  BatchOverlapFunction,
  BatchProportionalOverlap,
  PathOverlapLookup,
  PathOverlapLookupType
}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

sealed trait BatchOverlapFunctionConfig

object BatchOverlapFunctionConfig {

  final case object NoFilter extends BatchOverlapFunctionConfig

  final case class AgentProportional(
    pathOverlapLookupType: PathOverlapLookupType,
    useLinkWeights: Boolean
  ) {

    def build(): BatchOverlapFunction = {
      val pathOverlapLookup: PathOverlapLookup = pathOverlapLookupType match {
        case PathOverlapLookupType.TSP =>
          PathOverlapLookupType.trueShortestPathLookup(useLinkWeights)
        case PathOverlapLookupType.AllPaths =>
          PathOverlapLookupType.altPathsLookup(useLinkWeights)
      }
      (alts: Map[Request, List[Path]]) =>
        AgentProportionalBatchOverlap.from(alts, pathOverlapLookup)
    }
  }

  final case class BatchProportional(
    pathOverlapLookupType: PathOverlapLookupType,
    useLinkWeights: Boolean
  ) {

    def build(): BatchOverlapFunction = {
      val pathOverlapLookup: PathOverlapLookup = pathOverlapLookupType match {
        case PathOverlapLookupType.TSP =>
          PathOverlapLookupType.trueShortestPathLookup(useLinkWeights)
        case PathOverlapLookupType.AllPaths =>
          PathOverlapLookupType.altPathsLookup(useLinkWeights)
      }
      (alts: Map[Request, List[Path]]) =>
        BatchProportionalOverlap.from(alts, pathOverlapLookup)
    }
  }
}
