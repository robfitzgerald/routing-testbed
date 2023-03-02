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
  theta: Double,
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
                  requests = requests,
                  theta = theta
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

object UserOptimalAuctionSelection extends LazyLogging {

  /**
    * runs route selection for a batch of requests
    *
    * in the UserOptimalAuctionSelection setting, we dismiss k-shortest paths generation.
    * instead, we create only one new route which is optimal with respect to the current
    * network conditions. this is combined with the current route to make a selection
    * request for each agent.
    *
    * @param selectionRunner runs route selection algorithm
    * @param search routing algorithm, assumed optimal such as Dijkstra's
    * @param roadNetwork current network conditions
    * @param batchingManager current knowlege of route histories and batch-building function
    * @param bank current balance of money (karma) for each agent
    * @param zoneLookup mapping from a batch zone to the list of edges associated with the zone
    * @param batchId the id of this batch
    * @param requests the requests for this batch
    * @return the full routing algorithm response and all associated bank update
    */
  def resolveBatch(
    selectionRunner: SelectionRunner,
    search: (EdgeId, EdgeId, TraverseDirection) => IO[Option[Path]],
    roadNetwork: RoadNetworkIO,
    batchingManager: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]],
    batchId: String,
    requests: List[Request],
    theta: Double
  ): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    RoutingOps
      .findShortestPathForBatch(search, requests)
      .flatMap {
        case None            => IO.pure(None)
        case Some(withPaths) =>
          // to integrate with the existing framework, let's match the assumption that
          // there are at least two paths. for our case, path index 0 should be an updated
          // user-optimal path.
          // for the user optimal auction selection setting, we compare this updated UO route
          // with the current route. here, we explicitly place that current route in path
          // index 1.
          // because of this, we need to filter away any assignments that picked index 1.
          val withTwoPathsResult =
            withPaths
              .traverse {
                case (req, newSpur) =>
                  // we need to compare the provided faster path, starting/ending at the Request locations,
                  // with just a spur with the same start location
                  for {
                    hist        <- IO.fromEither(batchingManager.storedHistory.getNewestDataOrError(req.agent))
                    currentPath <- getCurrentPath(roadNetwork, hist)
                    currentSpur = pathSpurFromCurrent(newSpur, currentPath)
                    diverse <- sufficientlyDiverse(roadNetwork, currentPath, newSpur, theta)
                  } yield if (diverse) Some((req, List(newSpur, currentSpur))) else None
              }
              .map { twoPaths =>
                val numReqs    = requests.length
                val numDiverse = twoPaths.flatten.length
                if (numDiverse == 0) {
                  logger.info(s"batch $batchId has no requests that are sufficiently diverse")
                  None
                } else {
                  val msg = s"batch $batchId after generating new paths for agents, " +
                    s"$numDiverse/$numReqs are sufficiently diverse"
                  logger.info(msg)
                  Some(SelectionRunnerRequest(batchId, twoPaths.flatten.toMap))
                }
              }

          withTwoPathsResult.flatMap {
            case None => IO.pure(None)
            case Some(selectionRunnerRequest) =>
              for {
                selectionFn <- KarmaSelectionAlgorithmOps.instantiateSelectionAlgorithm(
                  selectionRunner,
                  roadNetwork,
                  batchingManager,
                  bank,
                  zoneLookup
                )(List(selectionRunnerRequest))
                selectionOutput <- KarmaSelectionAlgorithmOps.runSelectionWithBank(
                  List(selectionRunnerRequest),
                  roadNetwork,
                  selectionFn,
                  bank
                )
                (soResults, updatedBank) = selectionOutput
                oneResult <- RoutingOps.extractSingularBatchResult(soResults)
              } yield {
                // at this point, the underlying KarmaSelectionAlgorithm should have applied
                // the NetworkPolicyFilter so that we only see UO route responses

                oneResult
                  .map { selectionResult =>
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

  /**
    * grabs the current route and converts to a [[Path]] with latest network costs
    *
    * @param rn road network state
    * @param rrd current data stored for this route request
    * @return the latest route plan with updated network costs
    */
  def getCurrentPath(rn: RoadNetworkIO, rrd: AgentBatchData.RouteRequestData): IO[Path] =
    rrd.route.traverse { _.toPathSegment.updateCostEstimate(rn) }

  /**
    * picks a path spur from the current path which shares the same start location
    * as some newly-computed path spur (and assumed destination)
    *
    * @param newPathSpur
    * @param currentPath
    * @return
    */
  def pathSpurFromCurrent(newPathSpur: Path, currentPath: Path): Path = {

    newPathSpur match {
      case Nil            => currentPath
      case firstEdge :: _ => currentPath.dropWhile(_.edgeId != firstEdge.edgeId)
    }
  }

  /**
    * checks the overlap percentage (by distance) of the new path to the current path,
    * starting from the point in the future where they separate.
    *
    * @param rn current road network state
    * @param current the current path
    * @param proposed proposed path spur
    * @param theta overlap requirement to be "sufficiently diverse", read as
    *              the minimum amount of dis-similarity. for example, if theta
    *              is 33%, then the new path must have 33% of its distance that
    *              does not overlap with the current path.
    * @return the effect of finding if these paths are sufficiently diverse
    */
  def sufficientlyDiverse(rn: RoadNetworkIO, current: Path, proposed: Path, theta: Double): IO[Boolean] = {
    val refSpur       = pathSpurFromCurrent(proposed, current)
    val refDist       = refSpur.traverse(_.toEdgeData(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
    val proposedEdges = proposed.map(_.edgeId).toSet

    val overlapPercentResult = current.filter(e => proposedEdges.contains(e.edgeId)) match {
      case Nil => IO.pure(0.0)
      case overlapEdges =>
        for {
          refDist     <- refSpur.traverse(_.toEdgeData(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
          overlapDist <- overlapEdges.traverse(_.toEdgeData(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
        } yield if (refDist == 0.0) 0.0 else overlapDist / refDist
    }

    overlapPercentResult.map(_ > theta)
  }
}
