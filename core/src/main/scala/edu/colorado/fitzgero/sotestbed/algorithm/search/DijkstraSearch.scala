package edu.colorado.fitzgero.sotestbed.algorithm.search

import scala.annotation.tailrec

import cats.Monad
import cats.data.OptionT
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
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
  def vertexOrientedShortestPath[F[_]: Monad, V, E](
    roadNetworkModel: RoadNetwork[F, V, E],
    costFunction: E => Cost)(srcVertexId: VertexId, dstVertexId: VertexId, direction: TraverseDirection): F[Option[Path]] = {

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
        spanningTreeOption <- SpanningTree.vertexOrientedMinSpanningTree(roadNetworkModel, costFunction)(treeSource, Some {
          treeDestination
        }, direction)
      } yield {
        for {
          spanningTree <- spanningTreeOption
          path         <- backtrack(spanningTree)(treeDestination)
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
  def edgeOrientedShortestPath[F[_]: Monad, V, E](
    roadNetworkModel: RoadNetwork[F, V, E],
    costFunction: E => Cost)(srcEdgeId: EdgeId, dstEdgeId: EdgeId, direction: TraverseDirection): F[Option[Path]] = {
    if (srcEdgeId == dstEdgeId) {
      Monad[F].pure {
        Some { EmptyPath }
      }
    } else {

      val result: F[Option[Path]] = {
        for {
          dstVertexOfSrcEdge <- OptionT { roadNetworkModel.destination(srcEdgeId) }
          srcVertexOfDstEdge <- OptionT { roadNetworkModel.source(dstEdgeId) }
          result <- OptionT {
            vertexOrientedShortestPath(roadNetworkModel, costFunction)(dstVertexOfSrcEdge, srcVertexOfDstEdge, direction)
          }
        } yield PathSegment(srcEdgeId, Cost.Zero) +: result :+ PathSegment(dstEdgeId, Cost.Zero)
      }.value

      result
    }
  }

  /**
    * picks the minimum path from the provided starting point
    * @param spanningTree the constructed minimum cost tree
    * @param startPoint the vertex to start back tracking from
    * @return a path between the startPoint and the root of the spanning tree, if one exists
    */
  def backtrack[E](spanningTree: MinSpanningTree[E])(startPoint: VertexId): Option[Path] = {

    @tailrec def _backtrack(traversalVertex: VertexId, solution: List[MinSpanningTraversal[E]] = List.empty): List[MinSpanningTraversal[E]] = {

      spanningTree.traverse(traversalVertex) match {
        case None => solution
        case Some(MinSpanningTree.TraverseData(minSpanningTraversal, edgeTriplet)) =>
          val nextTraversalIndex: VertexId =
            spanningTree.traverseDirection match {
              case TraverseDirection.Forward => edgeTriplet.src
              case TraverseDirection.Reverse => edgeTriplet.dst
            }
          val updatedSolution: List[MinSpanningTraversal[E]] = minSpanningTraversal +: solution
          _backtrack(nextTraversalIndex, updatedSolution)
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
