package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.IO
import cats.{Applicative, Monad}
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.search.DijkstraSearch
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork, TraverseDirection}

case class SelfishSyncRoutingBPR(
  marginalCostFunction: EdgeBPR => Flow => Cost, // Likely EdgeBPRCostOps.marginalCostFunction
  pathToMarginalFlowsFunction: FlowObservationOps.PathToMarginalFlows[IO, Coordinate, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow
) extends RoutingAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  /**
    * runs a selfish routing search using the current network state
    * @param requests requests to process
    * @param activeAgentHistory ignored
    * @param roadNetwork current road network state
    * @return solution
    */
  def route(
    requests: List[Request],
    activeAgentHistory: ActiveAgentHistory,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[RoutingAlgorithm.Result] = {

    val costFlowsFunction: EdgeBPR => Cost = (edge: EdgeBPR) => marginalCostFunction(edge)(edge.flow)
    val search =
      DijkstraSearch.edgeOrientedShortestPath[IO, Coordinate, EdgeBPR](roadNetwork, costFlowsFunction) _

    // run Dijkstra's Search for each Request
    // IO[List[Option[(Request, Path, SelectionCost)]]]
    val searchResultsF = requests.traverse { req =>
      val thisSearchResult = for {
        pathOpt <- search(req.location, req.destination, TraverseDirection.Forward)
      } yield {
        val cost = pathOpt match {
          case Some(path) =>
            val selfishAssignmentCost: IO[SelectionCost] =
              SelectionAlgorithm
                .evaluateCostOfSelection(
                  List(path),
                  roadNetwork,
                  pathToMarginalFlowsFunction,
                  combineFlowsFunction,
                  marginalCostFunction
                )

            for {
              cost <- selfishAssignmentCost
            } yield {
              Some((req, path, cost))
            }
          case None =>
            IO.pure(None)
        }
        cost
      }
      thisSearchResult.flatten
    }

    // package as RoutingResult
    for {
      searchResults <- searchResultsF
    } yield {

      val successfulRouteResponses: List[Response] =
        searchResults.flatMap {
          case Some((request, path, selectionCost)) =>
            Some(Response(request, 0, path.map { _.edgeId }, selectionCost.overallCost))
          case None =>
            None
        }

      val alternatives: Map[Request, List[Path]] = {
        for {
          (request, path, _) <- searchResults.flatten
        } yield {
          request -> List(path)
        }
      }.toMap

      val selfishCostTotal: Cost = searchResults
        .foldLeft(Cost.Zero) { (acc, rOpt) =>
          val thisCost: Cost = rOpt.map { case (_, _, cost) => cost.overallCost }.getOrElse(Cost.Zero)
          acc + thisCost
        }

      val result = RoutingAlgorithm.Result(
        kspResult = alternatives,
        responses = successfulRouteResponses,
        kspRuntime = RunTime.Zero,
        selectionRuntime = RunTime.Zero,
        selfishCost = selfishCostTotal
      )

      result
    }
  }
}
