package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{TraverseDirection, VertexId}

case class MinSpanningTree(
  traverseDirection: TraverseDirection,
  tree: Map[VertexId, MinSpanningTraversal] = Map.empty
) {
  def traverseFrom(vertexId: VertexId): Option[MinSpanningTraversal] = tree.get(vertexId)
}
