package edu.colorado.fitzgero.sotestbed.algorithm.search

import cats.Monad
import cats.data.OptionT
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, RoadNetwork, TraverseDirection, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.TraverseDirection._
import com.typesafe.scalalogging.LazyLogging

object SpanningTree extends LazyLogging {

  /**
    * builds a Dijkstra's spanning tree rooted at a vertex, optionally with a stopping point
    * @param roadNetworkModel an instance of the RoadNetworkModel trait
    * @param costFunction a user-defined cost function from the provided edge type E
    * @param srcEdgeId the source of the path search
    * @param dstEdgeId the destination of the path search
    * @param direction traversing forward or reverse on edges (determines which edge is used for the tree root)
    * @tparam V vertex type
    * @tparam E edge type
    * @return
    */
  def edgeOrientedSpanningTree[F[_]: Monad, V, E](
    roadNetworkModel: RoadNetwork[F, V, E],
    costFunction: E => Cost,
    srcEdgeId: EdgeId,
    dstEdgeId: EdgeId,
    direction: TraverseDirection
  ): F[Option[MinSpanningTree[E]]] = {
    if (srcEdgeId == dstEdgeId) {
      Monad[F].pure {
        None
      }
    } else {

      // pick from edge triplet depending on direction
      val rootVertexT: F[Option[VertexId]] =
        if (direction == Forward) roadNetworkModel.destination(srcEdgeId)
        else roadNetworkModel.source(dstEdgeId)
      val dstVertexT: F[Option[VertexId]] =
        if (direction == Forward) roadNetworkModel.source(dstEdgeId)
        else roadNetworkModel.destination(srcEdgeId)

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
    frontier: collection.immutable.SortedSet[(RoadNetwork.EdgeTriplet[E], Cost)],
    solution: MinSpanningTree[E],
    explored: Set[VertexId],
    foundTarget: Boolean = false
  )

  def frontierOrdering[E]: Ordering[(RoadNetwork.EdgeTriplet[E], Cost)] =
    Ordering.by { case (triplet, cost) => (cost.value, triplet.hashCode) }

  object TreeBuildingAccumulator {

    def buildInitialAccumulator[F[_]: Monad, V, E](
      source: VertexId,
      direction: TraverseDirection,
      roadNetwork: RoadNetwork[F, V, E],
      costFunction: E => Cost
    ): F[TreeBuildingAccumulator[E]] = {

      implicit val frontierOrd: Ordering[(RoadNetwork.EdgeTriplet[E], Cost)] = frontierOrdering

      for {
        edgeTriplets <- roadNetwork.incidentEdgeTriplets(source, direction)
      } yield {
        val edgeTripletsWithCost = edgeTriplets.map { t => (t, Cost.Zero) }
        TreeBuildingAccumulator.apply[E](
          frontier = collection.immutable.SortedSet(edgeTripletsWithCost: _*),
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
    costFunction: E => Cost
  )(src: VertexId, dst: Option[VertexId], direction: TraverseDirection): F[Option[MinSpanningTree[E]]] = {

    implicit val frontierOrd: Ordering[(RoadNetwork.EdgeTriplet[E], Cost)] = frontierOrdering
    val stateUpdateFn                                                      = updateState(roadNetwork, costFunction) _

    // pulls the shortest frontier link
    val searchResult = TreeBuildingAccumulator
      .buildInitialAccumulator(src, direction, roadNetwork, costFunction)
      .flatMap {
        _.iterateUntilM { state =>
          state.frontier.headOption match {
            case None                    => Monad[F].pure { state }
            case Some((fTriplet, fCost)) =>
              // extract some details about this traversal
              val linkCost                  = costFunction(fTriplet.attr)
              val traversalVertex: VertexId = if (direction == Forward) fTriplet.dst else fTriplet.src
              val thisTraversal: MinSpanningTraversal[E] =
                MinSpanningTraversal(Some(fTriplet), traversalVertex, linkCost, fCost + linkCost)

              // perform the correct update depending on whether there exists a destination and if has been reached
              dst match {
                case Some(dstVertex) if dstVertex == traversalVertex =>
                  Monad[F].pure(reachedDestination(state, thisTraversal))
                case _ => stateUpdateFn(state, thisTraversal, direction)
              }
          }
        }(state => state.frontier.isEmpty || state.foundTarget)
      }

    searchResult.map { result => if (dst.isEmpty || result.foundTarget) Some(result.solution) else None }
  }

  /**
    * found our destination, we can skip expansion of the frontier and quit
    *
    * @param state
    * @param finalTraversal
    * @return
    */
  def reachedDestination[E](
    state: TreeBuildingAccumulator[E],
    finalTraversal: MinSpanningTraversal[E]
  ): TreeBuildingAccumulator[E] =
    state.copy(
      frontier = collection.immutable.SortedSet.empty[(RoadNetwork.EdgeTriplet[E], Cost)](frontierOrdering),
      solution = state.solution.append(finalTraversal),
      foundTarget = true
    )

  /**
    * expand our frontier and update the search state. each link in the frontier will be given the cost of
    * the traversed link (the cost to reach the link). the solution will be updated with this traversal.
    *
    * @param roadNetwork
    * @param costFunction
    * @param state
    * @param traversal
    * @param direction
    * @return
    */
  def updateState[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E], costFunction: E => Cost)(
    state: TreeBuildingAccumulator[E],
    traversal: MinSpanningTraversal[E],
    direction: TraverseDirection
  ) = {
    val hasBeenVisitedFn = hasBeenVisited(state, direction) _
    for {
      incidentEdgeTriplets <- roadNetwork.incidentEdgeTriplets(traversal.traversalVertex, direction)
      unvisitedDestinations = incidentEdgeTriplets.filterNot(hasBeenVisitedFn)
      nextFrontier          = unvisitedDestinations.map { t => (t, traversal.pathCost) }
    } yield state.copy(
      frontier = state.frontier.tail ++ nextFrontier,
      solution = state.solution.append(traversal),
      explored = state.explored + traversal.traversalVertex
    )
  }

  def hasBeenVisited[E](state: TreeBuildingAccumulator[E], direction: TraverseDirection)(
    triplet: RoadNetwork.EdgeTriplet[E]
  ): Boolean = {
    val tripletDst = if (direction == Forward) triplet.dst else triplet.src
    state.explored.contains(tripletDst)
  }
}
