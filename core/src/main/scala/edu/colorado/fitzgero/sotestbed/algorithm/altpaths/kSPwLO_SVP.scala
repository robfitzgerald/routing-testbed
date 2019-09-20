package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}

class kSPwLO_SVP[F[_] : Monad, V, E] extends AltPathsAlgorithm[F, V, E] {

  def generateAlts(requests: List[Request],
    roadNetworkModel: RoadNetwork[F, V, E],
    endTime: Option[RunTime]): F[(Map[Request, List[EdgeId]], Option[RunTime])] = {

    if (requests.isEmpty) Monad[F].pure { (Map.empty, None) }
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
