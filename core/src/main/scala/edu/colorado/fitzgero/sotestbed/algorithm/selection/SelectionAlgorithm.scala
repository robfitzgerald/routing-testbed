package edu.colorado.fitzgero.sotestbed.algorithm.selection
import cats.Monad

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork}

abstract class SelectionAlgorithm[F[_] : Monad, V, E] {

  def selectRoutes(alts: Map[Request, List[EdgeId]],
                   roadNetwork: RoadNetwork[F, V, E],
                   endTime: Option[RunTime]): F[(List[Response], Option[RunTime])]
}

// hey, here's a few ideas for a SelectionAlgorithm:
//   - the number of agents per selection algorithm is capped, but, multiples are run simultaneously, and the "best" solutions are resolved afterward
//   - groupings by overlap/visitation to common geo-cells