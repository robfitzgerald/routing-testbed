package edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetworkState}

class kSPwLO_SVP[V, E] extends AltPathsAlgorithm[V, E] {

  def generateAlts(requests: Seq[Request],
    roadNetworkModel: RoadNetworkState[V, E],
    endTime: Option[RunTime]): (Map[Request, Seq[EdgeId]], Option[RunTime]) = {

    if (requests.isEmpty) (Map.empty, None)
    else {
      val startTime: Option[Long] = endTime match {
        case Some(_) => Some { System.currentTimeMillis }
        case None => None
      }

      // min spanning tree forward

      // min spanning tree backward

      // for each node, store the svp distance as the fwd dist + bwd dist, and store combined path
      //  place in a heap

      // go through shortest -> longest, testing overlap according to written algorithm

      // TODO: we can't do this without a VertexId and vertex ops in RoadNetworkModel

      ???

    }
  }
}
