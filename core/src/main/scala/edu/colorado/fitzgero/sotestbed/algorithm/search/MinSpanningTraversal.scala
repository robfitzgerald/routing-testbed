package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeTriplet
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment, VertexId}

/**
  * wraps data related to traversing this edge in the context of a spanning tree build or traversal
  *
  * @param traversalEdgeTriplet the triplet from the road network resource representing this edge
  * @param traversalVertex looked up during traversals (dst vertex for forward trees, src vertex for reverse trees)
  * @param linkCost cost of only this edge
  * @param pathCost cost of path from spanning tree root to this edge
  * @tparam E link attribute type
  */
final case class MinSpanningTraversal[E](
  traversalEdgeTriplet: Option[EdgeTriplet[E]],
  traversalVertex     : VertexId,
  linkCost            : Cost,
  pathCost            : Cost
) {

  def toPathSegment: Option[PathSegment] = traversalEdgeTriplet.map { edgeTriplet =>
    PathSegment(edgeTriplet.edgeId, linkCost)
  }

  override def toString: String = {
    val edgeTripletString: String = traversalEdgeTriplet match {
      case Some(edgeTriplet) => edgeTriplet.toString
      case None              => "~"
    }
    s"v $traversalVertex: $edgeTripletString has path cost $pathCost"
  }
}
