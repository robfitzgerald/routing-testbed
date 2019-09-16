package edu.colorado.fitzgero.sotestbed.algorithm.selection
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetworkState}

trait SelectionAlgorithm[V, E] {

  def selectRoutes(alts: Map[Request, Seq[EdgeId]],
                   roadNetworkModel: RoadNetworkState[V, E],
                   endTime: Option[RunTime]): (List[Response], Option[RunTime])
}

// hey, here's a few ideas for a SelectionAlgorithm:
//   - the number of agents per selection algorithm is capped, but, multiples are run simultaneously, and the "best" solutions are resolved afterward
//   - groupings by overlap/visitation to common geo-cells