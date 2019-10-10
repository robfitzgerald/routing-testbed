package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{RoadNetwork, TraverseDirection, VertexId}

final case class MinSpanningTree[E](
  traverseDirection: TraverseDirection,
  tree: Map[VertexId, MinSpanningTraversal[E]] = Map.empty[VertexId, MinSpanningTraversal[E]]
) {
  def traverse(vertexId: VertexId): Option[MinSpanningTree.TraverseData[E]] =
    for {
      minSpanningTraversal <- tree.get(vertexId)
      edgeTriplet <- minSpanningTraversal.traversalEdgeTriplet
    } yield {
      MinSpanningTree.TraverseData(minSpanningTraversal, edgeTriplet)
    }
}

object MinSpanningTree {
  final case class TraverseData[E](minSpanningTraversal: MinSpanningTraversal[E], edgeTriplet: RoadNetwork.EdgeTriplet[E])
}