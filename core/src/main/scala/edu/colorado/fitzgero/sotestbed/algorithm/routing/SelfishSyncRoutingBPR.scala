package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.SyncIO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.search.DijkstraSearch
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, RunTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork, TraverseDirection}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

case class SelfishSyncRoutingBPR[V](
  marginalCostFunction: EdgeBPR => Flow => Cost, // Likely EdgeBPRCostOps.marginalCostFunction
  pathToMarginalFlowsFunction: RoutingOps.PathToMarginalFlows[SyncIO, V, EdgeBPR],
  combineFlowsFunction: Iterable[Flow] => Flow,
) extends RoutingAlgorithm[SyncIO, V, EdgeBPR] with LazyLogging {
  def route(requests: List[Request], roadNetwork: RoadNetwork[SyncIO, V, EdgeBPR]): SyncIO[RoutingAlgorithm.Result] = {

    val costFlowsFunction: EdgeBPR => Cost = (edge: EdgeBPR) => marginalCostFunction(edge)(edge.flow)
    val dijkstraSearch: (EdgeId, EdgeId, TraverseDirection) => SyncIO[Option[Path]] =
      DijkstraSearch.edgeOrientedShortestPath(roadNetwork, costFlowsFunction)

    // run Dijkstra's Search for each Request
    val searchResultsF: SyncIO[List[Option[(Request, Path, SelectionCost)]]] = requests.traverse{ req =>
      for {
        pathOpt <- dijkstraSearch(req.origin, req.destination, TraverseDirection.Forward)
      } yield {
        for {
          path <- pathOpt
        } yield {

          val selfishAssignmentCost: SelectionCost =
            SelectionAlgorithm.evaluateCostOfSelection(
              List(path),
              roadNetwork,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            ).unsafeRunSync()
          (req, path, selfishAssignmentCost)
        }
      }
    }

    // package as RoutingResult
    for {
      searchResults <- searchResultsF
    } yield {

      val successfulRouteResponses: List[Response] =
        searchResults.flatten.map{ case (request, path, selectionCost) =>
          Response(request, path.map{_.edgeId}, selectionCost.overallCost)
        }

      val alternatives: Map[Request, List[Path]] = {
        for {
          (request, path, _) <- searchResults.flatten
        } yield {
          request -> List(path)
        }
      }.toMap

      RoutingAlgorithm.Result(
        alternatives = alternatives,
        responses = successfulRouteResponses,
        kspRuntime = RunTime.Zero,
        selectionRuntime = RunTime.Zero
      )
    }
  }
}
