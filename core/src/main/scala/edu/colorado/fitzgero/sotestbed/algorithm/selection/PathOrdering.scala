package edu.colorado.fitzgero.sotestbed.algorithm.selection

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

sealed trait PathOrdering

object PathOrdering {

  case object TravelTimeAscending extends PathOrdering {

    def ordering: Ordering[Path] = Ordering.by { path =>
      val rank: Double = if (path.isEmpty) 0.0 else path.map { _.cost.value }.sum
      rank
    }
  }

  /**
    * ranks path alternatives based on overlap.
    *
    * for each edge in a path, collect the sum of visits from paths in the batch. sum all
    * such counts to a rank value, which is typically negated, which in turn orders paths
    * in greatest-overlap-first ordering.
    *
    * @param ascending optionally set the opposite least-overlap-first ordering.
    */
  case class BatchProportionalOverlapCount(ascending: Option[Boolean]) extends PathOrdering {

    def ordering(alts: Map[Request, List[Path]]): Ordering[Path] = {
      val lookup: Map[EdgeId, Int] = alts.values.flatten.flatten.groupBy { _.edgeId }.mapValues { _.size }
      Ordering.by { path =>
        if (path.isEmpty) 0.0
        else {
          val rankValue: Double = path.flatMap { pathSeg => lookup.get(pathSeg.edgeId) }.sum.toDouble
          val rank              = if (ascending.exists { identity }) rankValue else -rankValue
          rank
        }
      }
    }

  }
}
