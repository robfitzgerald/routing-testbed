package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

case class PathSegment(edgeId: EdgeId, cost: Cost)

object PathSegment {
  type Path = List[PathSegment]
  val EmptyPath = List.empty[PathSegment]
}
