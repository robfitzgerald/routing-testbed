package edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import cats.Monad

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}

abstract class AltPathsAlgorithm[F[_] : Monad, V, E] {

  def generateAlts(requests: List[Request],
                   roadNetwork: RoadNetwork[F, V, E],
                   endTime: Option[RunTime]): F[(Map[Request, List[EdgeId]], Option[RunTime])]
}
