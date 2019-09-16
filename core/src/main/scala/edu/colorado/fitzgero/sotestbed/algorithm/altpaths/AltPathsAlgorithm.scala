package edu.colorado.fitzgero.sotestbed.algorithm.altpaths
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetworkState}

trait AltPathsAlgorithm[V, E] {

  def generateAlts(requests: Seq[Request],
                   roadNetworkModel: RoadNetworkState[V, E],
                   endTime: Option[RunTime]): (Map[Request, Seq[EdgeId]], Option[RunTime])
}
