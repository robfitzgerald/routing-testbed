package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

object AgentProportionalBatchOverlap {

  /**
    * computes the overlap percent for each agent in the batch as x/y
    * where
    *    x = the link weights for any links overlapping any other agents
    *    y = the link weights for all links for this agent
    *
    * @param filteredAlts the filtered shortest path sets for this batch
    * @param pathOverlapLookup the type of lookup to apply
    * @return the calculated overlap
    */
  def from(filteredAlts: Map[Request, List[Path]], pathOverlapLookup: PathOverlapLookup): BatchOverlapResult = {

    val lookup: Map[Request, List[(EdgeId, Cost)]] =
      PathOverlapLookupType.buildLookup(filteredAlts, pathOverlapLookup)

    val result: Iterable[(Request, Double)] = for {
      (request, paths) <- filteredAlts
      thisAgentLinks = pathOverlapLookup(paths)
      otherAgents    = filteredAlts - request
    } yield {
      val otherRequestLinks = for {
        (otherRequest, _) <- otherAgents
        otherRequestLinks <- lookup.get(otherRequest)
      } yield {
        otherRequestLinks
      }
      val otherRequestLookup = otherRequestLinks.flatten.toMap

      // for each link in the agent's path set, find if there is overlap
      val (overlapCost, totalCost) =
        thisAgentLinks.foldLeft((Cost.Zero, Cost.Zero)) {
          case ((overlapAcc, totalAcc), (edgeId, cost)) =>
            otherRequestLookup.get(edgeId) match {
              case None =>
                val updatedTotalAcc = totalAcc + cost
                (overlapAcc, updatedTotalAcc)
              case Some(_) =>
                val updatedOverlapAcc = overlapAcc + cost
                val updatedTotalAcc   = totalAcc + cost
                (updatedOverlapAcc, updatedTotalAcc)
            }
        }

      // compute the overlap proportion for this request
      val overlapProportion = if (totalCost == Cost.Zero) 0.0 else overlapCost.value / totalCost.value
      request -> overlapProportion
    }

    BatchOverlapResult(result.toMap)
  }
}
