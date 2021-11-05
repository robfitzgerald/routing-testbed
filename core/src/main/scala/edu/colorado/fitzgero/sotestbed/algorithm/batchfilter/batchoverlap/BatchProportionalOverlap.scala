package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

object BatchProportionalOverlap {

  /**
    * computes the overlap percent for each agent in the batch as x/y
    * where
    *    x = the link weights for any links overlapping any other agents
    *    y = the link weights for all links observed by any agent
    *
    * @param filteredAlts the filtered shortest path sets for this batch
    * @param pathOverlapLookup the type of lookup to apply
    * @return the calculated overlap
    */
  def from(
    filteredAlts: Map[Request, List[Path]],
    pathOverlapLookup: PathOverlapLookup,
    overlapCostType: OverlapCostType
  ): BatchOverlapResult = {

    val lookup: Map[Request, List[(EdgeId, Cost)]] =
      PathOverlapLookupOps.buildLookup(filteredAlts, pathOverlapLookup)

    // create iterable with each edgeId for each request appearing exactly once along with its cost
    val allAgentsLinks: Map[EdgeId, Cost] = lookup.values.flatten.toMap

    val result: Iterable[(Request, Double)] = for {
      (request, paths) <- filteredAlts
      thisAgentLinks = pathOverlapLookup(paths).toMap
//      otherAgents    = filteredAlts - request
    } yield {
//      val otherRequestLinks = for {
//        (otherRequest, _) <- otherAgents
//        otherRequestLinks <- lookup.get(otherRequest)
//      } yield {
//        otherRequestLinks
//      }
      val (overlapCost, totalCost) =
        allAgentsLinks.foldLeft((Cost.Zero, Cost.Zero)) {
          case ((overlapAcc, totalAcc), (edgeId, cost)) =>
            thisAgentLinks.get(edgeId) match {
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
      val agentOverlapCost = overlapCostType.cost(overlapCost, totalCost)

      request -> agentOverlapCost
    }

    BatchOverlapResult(result.toMap)
  }
}
