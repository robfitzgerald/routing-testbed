package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import scala.annotation.tailrec

import cats.Monad
import cats.data.OptionT

import edu.colorado.fitzgero.sotestbed.algorithm.search.{DijkstraSearch, SpanningTree}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, PathSegment, RoadNetwork, TraverseDirection}

object kSPwLO_SVP_Algorithm {

  final case class SingleSVPResult(request: Request, alts: List[Path], pathsSeen: NaturalNumber)

  val ExhaustiveSearchTerminationFunction: AltPathsAlgorithm.AltPathsState => Boolean = _ => false

  def generateAltsForRequest[F[_]: Monad, V, E](
    request: Request,
    roadNetwork: RoadNetwork[F, V, E],
    costFunction: E => Cost,
    theta: Cost,
    terminationFunction: AltPathsAlgorithm.AltPathsState => Boolean = ExhaustiveSearchTerminationFunction
  ): F[Option[SingleSVPResult]] = {

    val startTime: Long = System.currentTimeMillis

    for {
      fwdTree <- OptionT {
        SpanningTree.edgeOrientedSpanningTree(roadNetwork, costFunction, request.origin, request.destination, TraverseDirection.Forward)
      }
      revTree <- OptionT {
        SpanningTree.edgeOrientedSpanningTree(roadNetwork, costFunction, request.destination, request.origin, TraverseDirection.Reverse)
      }
    } yield {

      val intersectionVertices: List[AltPathsAlgorithm.VertexWithDistance] = {
        for {
          vertexId <- fwdTree.tree.keys.toSet.intersect(revTree.tree.keys.toSet).toList
          cost = fwdTree.tree(vertexId).pathCost + revTree.tree(vertexId).pathCost
        } yield {
          AltPathsAlgorithm.VertexWithDistance(vertexId, cost)
        }
        }.sortBy { _.cost }

      val startState: AltPathsAlgorithm.AltPathsState = AltPathsAlgorithm.AltPathsState(intersectionVertices, startTime)

      // for each node, store the svp distance as the fwd dist + bwd dist, and store combined path
      //  place in a heap

      // go through shortest -> longest, testing overlap according to written algorithm

      @tailrec
      def _svp(searchState: AltPathsAlgorithm.AltPathsState): AltPathsAlgorithm.AltPathsState = {
        if (terminationFunction(searchState)) searchState
        else {
          searchState.intersectionVertices.headOption match {
            case None                                                                           => searchState
            case Some(AltPathsAlgorithm.VertexWithDistance(thisIntersectionVertexId, thisCost)) =>
              // construct a path from the intersection through both spanning trees
              val possiblyUpdatedState: Option[AltPathsAlgorithm.AltPathsState] = for {
                fwdPath <- DijkstraSearch.backtrack(fwdTree)(thisIntersectionVertexId)
                revPath <- DijkstraSearch.backtrack(revTree)(thisIntersectionVertexId)
                thisPath: Path = fwdPath ++ revPath
                hasCycles      = thisPath.map { _.edgeId }.toSet.size < thisPath.length
                if !hasCycles
              } yield {

                // test for similarity with all previous paths found
                val similarities: Iterable[Cost] = for {
                  (altPath, altCost) <- searchState.alts
                } yield {
                  if (altCost < thisCost) costOfOverlap(thisPath, altPath) / thisCost
                  else costOfOverlap(altPath, thisPath) / altCost
                }

                val sufficientlyDissimilar: Boolean = similarities.forall { _ < theta }

                if (sufficientlyDissimilar) {
                  val addToAlts: (Path, Cost) = (thisPath, thisCost)
                  searchState.copy(
                    intersectionVertices = searchState.intersectionVertices.tail,
                    alts = addToAlts +: searchState.alts,
                    pathsSeen = searchState.pathsSeen + NaturalNumber.One
                  )
                } else {
                  searchState.copy(
                    intersectionVertices = searchState.intersectionVertices.tail,
                    pathsSeen = searchState.pathsSeen + NaturalNumber.One
                  )
                }
              }

              possiblyUpdatedState match {
                case None =>
                  // failed for one of these possible reasons:
                  //  1. unable to backtrack from the vertex to create a path
                  //  2. the constructed single-via path had a cycle
                  val nextState = searchState.copy(
                    intersectionVertices = searchState.intersectionVertices.tail,
                    pathsSeen = searchState.pathsSeen + NaturalNumber.One
                  )
                  _svp(nextState)
                case Some(updatedState) =>
                  _svp(updatedState)
              }
          }
        }
      }

      val AltPathsAlgorithm.AltPathsState(_, _, pathsSeen, altsWithCosts) = _svp(startState)

      // sort in order discovered which should also be a sort by cost; only return the path
      val alts: List[Path] = altsWithCosts.reverse.map { case (path, _) => path }

      SingleSVPResult(request, alts, pathsSeen)
    }
    }.value

  /**
    * computes the sum of Costs of overlap for two paths, one which is given to be longer than the other
    *
    * @param longerPath the path of the two which has a higher overall Cost
    * @param shorterPath the path of the two which has a lower overall Cost
    * @return the sum of Costs for links which overlap
    */
  def costOfOverlap(longerPath: Path, shorterPath: Path): Cost = {

    val altPathMap: Map[EdgeId, Cost] =
      longerPath.map { pathSegment =>
        (pathSegment.edgeId, pathSegment.cost)
      }.toMap

    val thisPathOverlapCost: Cost = {
      for {
        PathSegment(altEdgeId, altEdgeCost) <- shorterPath
        if altPathMap.isDefinedAt(altEdgeId)
      } yield {
        altEdgeCost
      }
      }.foldLeft(Cost.Zero) { _ + _ }

    thisPathOverlapCost
  }

}
