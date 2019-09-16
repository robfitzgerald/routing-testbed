package edu.colorado.fitzgero.sotestbed.algorithm.search

import scala.annotation.tailrec
import scala.collection.immutable

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.PathSegment.{EmptyPath, Path}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

object DijkstraSearch {

  /**
    * search for a path from source to destination which has minimal cost
    *
    * @param roadNetworkModel an instance of the RoadNetworkModel trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param srcVertexId the source of the path
    * @param dstVertexId the destination of the path
    * @param direction searching forward or backward from src to dst
    * @tparam V vertex type
    * @tparam E edge type
    * @return
    */
  def vertexOrientedShortestPath[V, E](roadNetworkModel: RoadNetworkState[V, E],
                                       costFunction: E => Cost)(
      srcVertexId: VertexId,
      dstVertexId: VertexId,
      direction: TraverseDirection): Option[Path] = {

    if (srcVertexId == dstVertexId) {
      Some { EmptyPath }
    } else {

      val (treeSource, treeDestination) =
        direction match {
          case TraverseDirection.Forward => (srcVertexId, dstVertexId)
          case TraverseDirection.Reverse => (dstVertexId, srcVertexId)
        }

      for {
        spanningTree <- vertexOrientedMinSpanningTree(roadNetworkModel, costFunction)(treeSource, Some {
          treeDestination
        }, direction)
        path <- backtrack(spanningTree)(treeDestination)
      } yield {
        path
      }
    }
  }

  /**
    * search for a path from source edge to destination edge which is minimal cost
    * @param roadNetworkModel an instance of the RoadNetworkModel trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param srcEdgeId the source of the path
    * @param dstEdgeId the destination of the path
    * @param direction searching forward or backward from src to dst
    * @tparam V vertex type
    * @tparam E edge type
    * @return
    */
  def edgeOrientedShortestPath[V, E](roadNetworkModel: RoadNetworkState[V, E],
                                     costFunction: E => Cost)(
      srcEdgeId: EdgeId,
      dstEdgeId: EdgeId,
      direction: TraverseDirection): Option[Path] = {
    for {
      srcVertex <- roadNetworkModel.destination(srcEdgeId)
      dstVertex <- roadNetworkModel.source(dstEdgeId)
    } yield {
      if (srcEdgeId != dstEdgeId) {
        // the agent is at both source and destination
        EmptyPath
      } else if (srcVertex == dstVertex) {

        // edges are incident, path is length 2 and can be constructed without a search
        val srcCost: Cost = roadNetworkModel.edge(srcEdgeId) match {
          case None       => Cost.Zero
          case Some(edge) => costFunction(edge)
        }
        val dstCost: Cost = roadNetworkModel.edge(dstEdgeId) match {
          case None       => Cost.Zero
          case Some(edge) => costFunction(edge)
        }
        List(PathSegment(srcEdgeId, srcCost), PathSegment(dstEdgeId, dstCost))
      } else {

        // search for shortest path
        vertexOrientedShortestPath(roadNetworkModel, costFunction)(srcVertex, dstVertex, direction)
          .getOrElse(EmptyPath)
      }
    }
  }




  /**
    * builds a Dijkstra's spanning tree rooted at a vertex, optionally with a stopping point
    *
    * @param roadNetworkModel an instance of the RoadNetworkModel trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param src root of the tree
    * @param dst optional stopping point; stop expanding when this vertex is added to the solution
    * @param direction searching forward or backward from src to dst
    * @tparam V vertex type
    * @tparam E edge type
    * @return
    */
  def vertexOrientedMinSpanningTree[V, E](roadNetworkModel: RoadNetworkState[V, E], costFunction: E => Cost)(
      src: VertexId,
      dst: Option[VertexId],
      direction: TraverseDirection): Option[MinSpanningTree] = {

    import MinSpanningTraversal.MinSpanningTraversalOrdering

    @scala.annotation.tailrec
    def _spanningTree(queue: immutable.SortedSet[MinSpanningTraversal],
                      solution: MinSpanningTree = MinSpanningTree(direction)): MinSpanningTree =
      queue.headOption match {
        case None => solution
        case Some(nextVertex) =>

          // get the neighbors of the next vertex
          val discoveredVertices: List[MinSpanningTraversal] = for {
            (edgeId, neighbor) <- roadNetworkModel.edgesAndNeighbors(nextVertex.connectingVertex, direction)
            edge <- roadNetworkModel.edge(edgeId)
            cost = costFunction(edge)
          } yield {
            MinSpanningTraversal(Some{ edgeId }, neighbor, cost)
          }

          // if we haven't added this vertex to the solution before, then, let's do it!
          val updatedSolution: MinSpanningTree =
            solution.tree.get(nextVertex.connectingVertex) match {
              case None =>
                solution.copy(
                  tree = solution.tree.updated(nextVertex.connectingVertex, nextVertex)
                )
              case Some(_) => solution
            }

          dst match {
            case None =>
              _spanningTree(queue ++ discoveredVertices, updatedSolution)
            case Some(dstVertex) =>
              if (nextVertex.connectingVertex == dstVertex) {
                // stop when we hit the provided solution
                updatedSolution
              } else {
                _spanningTree(queue ++ discoveredVertices, updatedSolution)
              }
          }


      }

    Some {
      _spanningTree(immutable.SortedSet(MinSpanningTraversal(None, src, Cost.Zero)))
    }
  }

  /**
    * picks the minimum path from the provided starting point
    * @param spanningTree the constructed minimum cost tree
    * @param startPoint the vertex to start back tracking from
    * @return a path between the startPoint and the root of the spanning tree, if one exists
    */
  def backtrack(spanningTree: MinSpanningTree)(startPoint: VertexId): Option[Path] = {

    @tailrec def _backtrack(
        traversalVertex: VertexId,
        solution: List[MinSpanningTraversal] = List.empty): List[MinSpanningTraversal] = {

      spanningTree.traverseFrom(traversalVertex) match {
        case None => solution
        case Some(minSpanningTraversal) =>
          val updatedSolution = minSpanningTraversal +: solution
          _backtrack(minSpanningTraversal.connectingVertex, updatedSolution)
      }
    }

    val path: Path = _backtrack(startPoint).flatMap { _.toPathSegment }

    // return path if it includes the requested source (do not return invalid paths)
    if (path.isEmpty) None
    else
      Some {
        spanningTree.traverseDirection match {
          case TraverseDirection.Forward => path
          case TraverseDirection.Reverse => path.reverse
        }
      }
  }
}
