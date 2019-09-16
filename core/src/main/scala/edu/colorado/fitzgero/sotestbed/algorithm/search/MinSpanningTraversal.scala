package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment, VertexId}

case class MinSpanningTraversal(traversalEdgeId: Option[EdgeId], connectingVertex: VertexId, cost: Cost) {
  def toPathSegment: Option[PathSegment] = traversalEdgeId.map{ edgeId => PathSegment(edgeId, cost) }
}

object MinSpanningTraversal {
  import edu.colorado.fitzgero.sotestbed.model.numeric.Cost.CostOrdering
  implicit val MinSpanningTraversalOrdering: Ordering[MinSpanningTraversal] = Ordering.by{_.cost}
}