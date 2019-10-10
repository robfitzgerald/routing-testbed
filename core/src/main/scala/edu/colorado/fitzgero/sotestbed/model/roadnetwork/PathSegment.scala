package edu.colorado.fitzgero.sotestbed.model.roadnetwork

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

case class PathSegment(edgeId: EdgeId, cost: Cost) {
  override def toString: String = s"PathSegment($edgeId, $cost)"
}
