package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

sealed trait PathOverlapLookupType

object PathOverlapLookupType {

  final case object TSP      extends PathOverlapLookupType
  final case object AllPaths extends PathOverlapLookupType

  /**
    * applies a PathOverlapLookup function to a set of filtered alternative paths
    * in order to produce a lookup table for determining batch overlap
    *
    * @param filteredAlts the batch alternative paths set
    * @param pathOverlapLookup the user-provided function for lookup
    * @return the lookup table
    */
  def buildLookup(filteredAlts: Map[Request, List[Path]], pathOverlapLookup: PathOverlapLookup): Map[Request, List[(EdgeId, Cost)]] = {
    val result = for {
      (request, paths) <- filteredAlts
    } yield request -> pathOverlapLookup(paths)
    result
  }

  /**
    * creates a true shortest path lookup table for computing
    * path overlap percent
    *
    * @param useLinkWeights if true, preserve the link weights, otherwise
    *                       count each link as having a cost of "1"
    * @return a lookup table for solving for path overlap
    */
  def trueShortestPathLookup(useLinkWeights: Boolean = true): PathOverlapLookup = { (paths: List[Path]) =>
    {
      paths.headOption match {
        case Some(tsp) =>
          tsp.map { seg =>
            seg.edgeId -> (if (useLinkWeights) seg.cost else Cost(1))
          }
        case None =>
          List.empty
      }
    }
  }

  /**
    * creates a lookup table for computing path overlap percent which
    * considers all possible edges across all alternative paths provided
    * for this agent
    *
    * @param useLinkWeights if true, preserve the link weights, otherwise
    *                       count each link as having a cost of "1"
    * @return a lookup table for solving for path overlap
    */
  def altPathsLookup(useLinkWeights: Boolean = true): PathOverlapLookup = { (paths: List[Path]) =>
    {
      val listOfEdgesAndCosts: List[(EdgeId, Cost)] = for {
        path <- paths
        seg  <- path
      } yield seg.edgeId -> (if (useLinkWeights) seg.cost else Cost(1))
      listOfEdgesAndCosts
    }
  }
}
