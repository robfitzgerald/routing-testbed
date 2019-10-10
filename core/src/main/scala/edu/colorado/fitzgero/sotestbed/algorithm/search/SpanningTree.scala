package edu.colorado.fitzgero.sotestbed.algorithm.search

import cats.Monad
import cats.data.OptionT
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork, TraverseDirection, VertexId}

object SpanningTree {

  /**
    * builds a Dijkstra's spanning tree rooted at a vertex, optionally with a stopping point
    * @param roadNetworkModel an instance of the RoadNetworkModel trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param rootEdgeId the root of the spanning tree we are building
    * @param dstEdgeId the destination of the path
    * @param direction traversing forward or reverse on edges
    * @tparam V vertex type
    * @tparam E edge type
    * @return
    */
  def edgeOrientedSpanningTree[F[_]: Monad, V, E](roadNetworkModel: RoadNetwork[F, V, E],
                                                  costFunction: E => Cost,
                                                  rootEdgeId: EdgeId,
                                                  dstEdgeId: EdgeId,
                                                  direction: TraverseDirection): F[Option[MinSpanningTree[E]]] = {
    if (rootEdgeId == dstEdgeId) {
      Monad[F].pure {
        None
      }
    } else {

      // pick from edge triplet depending on direction
      val rootVertexT: F[Option[VertexId]] =
        if (direction == TraverseDirection.Forward) roadNetworkModel.source(rootEdgeId)
        else roadNetworkModel.destination(rootEdgeId)
      val dstVertexT: F[Option[VertexId]] =
        if (direction == TraverseDirection.Forward) roadNetworkModel.destination(dstEdgeId)
        else roadNetworkModel.source(dstEdgeId)

      val result: F[Option[MinSpanningTree[E]]] = {
        for {
          srcVertex <- OptionT { rootVertexT }
          dstVertex <- OptionT { dstVertexT }
          result <- OptionT {
            vertexOrientedMinSpanningTree(roadNetworkModel, costFunction)(srcVertex, Some { dstVertex }, direction)
          }
        } yield {
          result
        }
      }.value

      result
    }
  }

  case class TreeBuildingAccumulator[E](
    frontier: List[(RoadNetwork.EdgeTriplet[E], Cost)],
    solution: MinSpanningTree[E],
    explored: Set[VertexId],
    foundTarget: Boolean = false
  )

  object TreeBuildingAccumulator {

    def buildInitialAccumulator[F[_]: Monad, V, E](
      source: VertexId,
      direction: TraverseDirection,
      roadNetwork: RoadNetwork[F, V, E],
      costFunction: E => Cost): F[TreeBuildingAccumulator[E]] = {

      for {
        edgeTriplets <- roadNetwork.incidentEdgeTriplets(source, direction)
      } yield {
        val edgeTripletsWithCost = edgeTriplets.map { t =>
          (t, costFunction(t.attr))
        }
        TreeBuildingAccumulator.apply[E](
          frontier = edgeTripletsWithCost,
          solution = MinSpanningTree[E](direction),
          explored = Set(source)
        )
      }

    }
  }

  /**
    * builds a spanning tree rooted at a vertex, optionally with a stopping point, using a breadth-first search
    *
    * @param roadNetwork an instance of the RoadNetwork trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param src root of the tree
    * @param dst optional stopping point; stop expanding when this vertex is added to the solution
    * @param direction searching forward or backward from src to dst
    * @tparam V vertex type
    * @tparam E edge type
    * @return a spanning tree rooted at src and explored via direction, but is forward-oriented
    */
  def vertexOrientedMinSpanningTree[F[_]: Monad, V, E](
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost)(src: VertexId, dst: Option[VertexId], direction: TraverseDirection): F[Option[MinSpanningTree[E]]] = {

    // define the ordering needed for the SortedSet objects
//    implicit val FrontierOrdering: Ordering[(RoadNetwork.EdgeTriplet[E], Cost)] = Ordering.by { tuple => tuple._2.value + (1.0 / tuple._1.edgeId.hashCode.toDouble) }

    // tree routed at search source
    val initialTree: F[TreeBuildingAccumulator[E]] =
      TreeBuildingAccumulator.buildInitialAccumulator(src, direction, roadNetwork, costFunction)

    // recursive tree search

    for {
      init <- initialTree
    } yield {

      // recursive spanning tree BFS
      val searchResultF: F[TreeBuildingAccumulator[E]] = init.iterateUntilM { state =>
        state.frontier.headOption match {
          case None =>
            Monad[F].pure { state }
          case Some((thisEdgeTriplet, thisEdgeTripletCost)) =>
            val traversalVertex: VertexId = if (direction == TraverseDirection.Forward) thisEdgeTriplet.dst else thisEdgeTriplet.src

            if (direction == TraverseDirection.Reverse) {
              initialTree
            }

            if (state.explored.contains(traversalVertex)) {

              // can only visit each vertex once!
              Monad[F].pure {
                state.copy(
                  frontier = state.frontier.tail
                )
              }
            } else {

              // get the neighbors of the next vertex
              for {
                incidentEdgeTriplets <- roadNetwork.incidentEdgeTriplets(traversalVertex, direction)
              } yield {

                val discoveredVertices: List[MinSpanningTraversal[E]] = for {
                  edgeTriplet <- incidentEdgeTriplets
                  nextVertexId            = if (direction == TraverseDirection.Forward) edgeTriplet.dst else edgeTriplet.src
                  frontierEdgeTripletCost = costFunction(edgeTriplet.attr)
                } yield {
                  MinSpanningTraversal(Some { edgeTriplet }, nextVertexId, thisEdgeTripletCost + frontierEdgeTripletCost)
                }

                // add this edge to the solution
                val thisMinSpanningTraversal: MinSpanningTraversal[E] =
                  MinSpanningTraversal(
                    Some { thisEdgeTriplet },
                    traversalVertex,
                    thisEdgeTripletCost
                  )

                val updatedSolution: MinSpanningTree[E] =
                  state.solution.copy(
                    tree = state.solution.tree.updated(traversalVertex, thisMinSpanningTraversal)
                  )

                val updatedFrontier: List[(RoadNetwork.EdgeTriplet[E], Cost)] =
                  for {
                    minSpanningTraversal <- discoveredVertices
                    edgeTriplet          <- minSpanningTraversal.traversalEdgeTriplet
                  } yield {
                    (edgeTriplet, minSpanningTraversal.cost)
                  }

                dst match {
                  case None =>
                    // no destination; continue to build spanning tree
                    state.copy(
                      frontier = state.frontier.tail ++ updatedFrontier,
                      solution = updatedSolution,
                      explored = state.explored + traversalVertex
                    )
                  case Some(dstVertex) =>
                    if (traversalVertex == dstVertex) {
                      // stop when we hit the provided solution
                      state.copy(
                        frontier = List.empty[(RoadNetwork.EdgeTriplet[E], Cost)],
                        solution = updatedSolution,
                        foundTarget = true
                      )
                    } else {
                      // no destination found; continue to build spanning tree
                      state.copy(
                        frontier = state.frontier.tail ++ updatedFrontier,
                        solution = updatedSolution,
                        explored = state.explored + traversalVertex
                      )
                    }
                }
              }
            }
        }
      }(state => state.frontier.headOption.isEmpty || state.foundTarget)

      val result: F[Option[MinSpanningTree[E]]] = for {
        searchResult <- searchResultF
      } yield {
        if (dst.isEmpty) {
          Some {
            searchResult.solution
          }
        } else {
          if (searchResult.foundTarget) {
            Some {
              searchResult.solution
            }
          } else {
            None
          }
        }
      }

      result
    }
  }.flatten // F[F[_]] -> F[_]
}
