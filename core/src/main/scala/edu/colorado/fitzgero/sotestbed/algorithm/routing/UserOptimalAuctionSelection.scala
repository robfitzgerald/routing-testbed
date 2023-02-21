package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkIO
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter._
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import cats.effect.IO
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.search.DijkstraSearch
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.TraverseDirection
import edu.colorado.fitzgero.sotestbed.algorithm.selection._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm._

final case class UserOptimalAuctionSelection(
  batchingFunction: BatchingFunction,
  selectionRunner: SelectionRunner,
  replanAtSameLink: Boolean,
  useCurrentLinkFlows: Boolean
) extends RoutingAlgorithmV3
    with LazyLogging {

  import UserOptimalAuctionSelection._

  def observedTravelTimeCost(edge: EdgeBPR): Cost = Cost(edge.observedTravelTime.value)

  /**
    * uses auctions to select agents which can receive an update to their
    * path, using a path generated based on the current network state.
    *
    * @param roadNetwork current road network state
    * @param requests requests for routing from a batch or sub-batch
    * @param currentSimTime the current time in the simulation
    * @param batchingManager service tracking batching information
    * @param bank the balances of each agent in the system
    * @return routing results for each batch along with the updated bank
    */
  def route(
    roadNetwork: RoadNetworkIO,
    requests: List[AgentBatchData.RouteRequestData],
    currentSimTime: SimTime,
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])] = {

    val search: (EdgeId, EdgeId, TraverseDirection) => IO[Option[Path]] =
      DijkstraSearch.edgeOrientedShortestPath[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR](
        roadNetwork,
        observedTravelTimeCost
      )

    // apply the batching strategy and then filter out batches based on some
    // basic rules like whether or not agents can replan at the same link as
    // a previous replanning
    val batchingResult =
      batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime).map {
        case None => (List.empty, Map.empty[String, List[EdgeId]])
        case Some(BatchingFunction.BatchingResult(routeRequests, zoneLookup)) =>
          val preFiltered = RoutingOps.preFilterBatches(
            batchingManager = batchingManager,
            minBatchSize = 1,
            replanAtSameLink = replanAtSameLink,
            batches = routeRequests
          )
          (preFiltered, zoneLookup)
      }

    val selectionResults = batchingResult.flatMap {
      case (batches, zones) =>
        val initial: IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])] =
          IO.pure((List.empty, bank))
        batches.foldLeft(initial) {
          case (acc, (batchId, requests)) =>
            acc.flatMap {
              case (prevResults, thisBank) =>
                resolveBatch(
                  selectionRunner = selectionRunner,
                  search = search,
                  roadNetwork = roadNetwork,
                  batchingManager = batchingManager,
                  bank = thisBank,
                  zoneLookup = zones,
                  batchId = batchId,
                  requests = requests
                ).flatMap {
                  case None => acc
                  case Some((routingResult, updatedBank)) =>
                    IO.pure(((batchId, routingResult) +: prevResults, updatedBank))
                }
            }
        }
    }

    selectionResults
  }
}

object UserOptimalAuctionSelection {

  def resolveBatch(
    selectionRunner: SelectionRunner,
    search: (EdgeId, EdgeId, TraverseDirection) => IO[Option[Path]],
    roadNetwork: RoadNetworkIO,
    batchingManager: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]],
    batchId: String,
    requests: List[Request]
  ): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    RoutingOps
      .findShortestPathForBatch(search, requests)
      .flatMap {
        case None => IO.pure(None)
        case Some(withPaths) =>
          val selectionReqs = SelectionRunnerRequest.fromTspResult(batchId, withPaths)
          for {
            selectionFn <- KarmaSelectionAlgorithmOps.instantiateSelectionAlgorithm(
              selectionRunner,
              roadNetwork,
              batchingManager,
              bank,
              zoneLookup
            )(List(selectionReqs))
            selectionOutput <- KarmaSelectionAlgorithmOps.runSelectionWithBank(
              List(selectionReqs),
              roadNetwork,
              selectionFn,
              bank
            )
            (soResults, updatedBank) = selectionOutput
            oneResult <- RoutingOps.extractSingularBatchResult(soResults)
          } yield {
            oneResult.map { selectionResult =>
              val result = RoutingAlgorithm.Result(
                responses = selectionResult.selection.selectedRoutes,
                agentHistory = batchingManager.storedHistory
              )
              (result, updatedBank)
            }
          }
      }
  }

}
