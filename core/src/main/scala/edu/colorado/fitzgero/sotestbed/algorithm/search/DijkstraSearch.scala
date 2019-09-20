package edu.colorado.fitzgero.sotestbed.algorithm.search

import scala.annotation.tailrec
import scala.collection.immutable

import cats.Monad
import cats.implicits._

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
  def vertexOrientedShortestPath[F[_] : Monad, V, E](roadNetworkModel: RoadNetwork[F, V, E],
                                       costFunction: E => Cost)(
      srcVertexId: VertexId,
      dstVertexId: VertexId,
      direction: TraverseDirection): F[Option[Path]] = {

    if (srcVertexId == dstVertexId) {
      Monad[F].pure {
        Some { EmptyPath }
      }
    } else {

      val (treeSource, treeDestination) =
        direction match {
          case TraverseDirection.Forward => (srcVertexId, dstVertexId)
          case TraverseDirection.Reverse => (dstVertexId, srcVertexId)
        }

      for {
        spanningTreeOption <- vertexOrientedMinSpanningTree(roadNetworkModel, costFunction)(treeSource, Some {
          treeDestination
        }, direction)
      } yield {
        for {
          spanningTree <- spanningTreeOption
          path <- backtrack(spanningTree)(treeDestination)
        } yield {
          path
        }
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
  def edgeOrientedShortestPath[F[_] : Monad, V, E](roadNetworkModel: RoadNetwork[F, V, E],
                                     costFunction          : E => Cost)(
      srcEdgeId: EdgeId,
      dstEdgeId: EdgeId,
      direction: TraverseDirection): F[Option[Path]] = {
    if (srcEdgeId == dstEdgeId) {
      Monad[F].pure{
        Some{EmptyPath}
      }
    } else {



      val result: F[F[Option[Path]]] = for {
        dstVertexOfSrcEdgeOption <- roadNetworkModel.destination(srcEdgeId)
        srcVertexOfDstEdgeOption <- roadNetworkModel.source(dstEdgeId)
      } yield {
        val result: Option[F[Option[Path]]] = for {
          dstVertexOfSrcEdge <- dstVertexOfSrcEdgeOption
          srcVertexOfDstEdge <- srcVertexOfDstEdgeOption
        } yield {
          if (dstVertexOfSrcEdge == srcVertexOfDstEdge) {

            // edges are incident, path is length 2 and can be constructed without a search
            val incidentPathResult: F[Option[Path]] = for {
              srcEdgeOption <- roadNetworkModel.edge(srcEdgeId)
              dstEdgeOption <- roadNetworkModel.edge(dstEdgeId)
            } yield {
              for {
                srcEdge <- srcEdgeOption
                dstEdge <- dstEdgeOption
              } yield {
                List(PathSegment(srcEdgeId, costFunction(srcEdge)), PathSegment(dstEdgeId, costFunction(dstEdge)))
              }
            }

            incidentPathResult

          } else {

            // search for shortest path
            val searchPathResult: F[Option[Path]] = for {
              pathOption <- vertexOrientedShortestPath(roadNetworkModel, costFunction)(dstVertexOfSrcEdge, srcVertexOfDstEdge, direction)
            } yield {
              pathOption
            }

            searchPathResult
          }
        }
      }

      result.flatten
    }
  }


  case class TreeBuildingAccumulator(
    queue: immutable.SortedSet[MinSpanningTraversal],
    solution: MinSpanningTree,
    foundTarget: Boolean = false
  )
  object TreeBuildingAccumulator {
    def apply(source: VertexId, direction: TraverseDirection)(implicit ord: Ordering[MinSpanningTraversal]): TreeBuildingAccumulator =
      TreeBuildingAccumulator(
        immutable.SortedSet(MinSpanningTraversal(None, source, Cost.Zero)),
        MinSpanningTree(direction)
      )
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
  def vertexOrientedMinSpanningTree[F[_] : Monad, V, E](roadNetworkModel: RoadNetwork[F, V, E], costFunction: E => Cost)(
      src: VertexId,
      dst: Option[VertexId],
      direction: TraverseDirection): F[Option[MinSpanningTree]] = {

    // tree routed at search source
    val initialTree: TreeBuildingAccumulator = TreeBuildingAccumulator(src, direction)(MinSpanningTraversal.MinSpanningTraversalOrdering)

    // recursive tree search
    val result: F[TreeBuildingAccumulator] = initialTree.iterateUntilM{ state =>
      state.queue.headOption match {
        case None =>
          Monad[F].pure{ state }
        case Some(nextVertex) =>

          // get the neighbors of the next vertex
          for {
            edgesAndNeighborsF <- roadNetworkModel.edgesAndNeighbors(nextVertex.connectingVertex, direction)
            edgesInNetwork <- edgesAndNeighborsF.traverse( tup => roadNetworkModel.edge(tup._1))
          } yield {

            val discoveredVertices: List[MinSpanningTraversal] = for {
              edge <- edgesInNetwork
              (edgeId, neighbor) <- edgesAndNeighborsF
              //              edgeF <- roadNetworkModel.edge(edgeId)
              //            edge <- edgeF
              cost = costFunction(edge)
            } yield {
              MinSpanningTraversal(Some{ edgeId }, neighbor, cost)
            }

            // if we haven't added this vertex to the solution before, then, let's do it!
            val updatedSolution: MinSpanningTree =
              state.solution.tree.get(nextVertex.connectingVertex) match {
                case None =>
                  state.solution.copy(
                    tree = state.solution.tree.updated(nextVertex.connectingVertex, nextVertex)
                  )
                case Some(_) => state.solution
              }

            dst match {
              case None =>
                state.copy(
                  queue = state.queue ++ discoveredVertices,
                  solution = updatedSolution
                )
              case Some(dstVertex) =>
                if (nextVertex.connectingVertex == dstVertex) {
                  // stop when we hit the provided solution
                  state.copy(
                    solution = updatedSolution,
                    foundTarget = true
                  )
                } else {
                  state.copy(
                    queue = state.queue ++ discoveredVertices,
                    solution = updatedSolution
                  )
                }
            }
          }
      }
    }(_.queue.headOption.isEmpty)

    // handle tree result
    // if the search requested a destination
    for {
      tree <- result
    } yield {
      if (dst.isEmpty) {
        Some {
          tree
        }
      } else {
        if (tree.foundTarget) {
          Some {
            tree
          }
        } else {
          None
        }
      }
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
