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
        val resolveFn = resolveBatch(
          selectionRunner = selectionRunner,
          search = search,
          roadNetwork = roadNetwork,
          batchingManager = batchingManager,
          zoneLookup = zones,
          theta = theta
        ) _

        val nBatches = batches.length
        logger.debug(s"running user optimal auction selection on $nBatches batches")

        case class State(
          batches: List[(String, List[Request])] = batches,
          responses: List[(String, RoutingAlgorithm.Result)] = List.empty,
          bank: Map[String, Karma] = bank
        ) {
          override def toString = s"State(${batches.length} batches, ${responses.length} responses)"
        }

        val runResult = State().iterateUntilM { state =>
          state.batches match {
            case Nil =>
              logger.debug(s"done running batches")
              IO.pure(state)
            case (batchId, requests) :: nextBatches =>
              val nRemaining = state.batches.length
              logger.debug(s"running batch $batchId ($nRemaining/$nBatches remaining) - run state: $state")
              for {
                response <- resolveFn(state.bank, batchId, requests)
              } yield {
                response match {
                  case None =>
                    logger.debug(s"batch $batchId yielded no responses, remaining: ${nextBatches.length}")
                    state.copy(nextBatches)
                  case Some((result, updatedBank)) =>
                    val msg = s"batch $batchId yielded ${result.responses.length} responses. " +
                      s"remaining batches: ${nextBatches.length}"
                    logger.debug(msg)
                    state.copy(nextBatches, (batchId, result) +: state.responses, updatedBank)
                }
              }
          }
        } { s =>
          val stop = s.batches.isEmpty
          logger.debug(s"iteration state: $s - stop iteration? $stop")
          stop
        }

        runResult.map { state => (state.responses, state.bank) }

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
    zoneLookup: Map[String, List[EdgeId]],
    theta: Double
  )(
    bank: Map[String, Karma],
    batchId: String,
    requests: List[Request]
  ): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    val filterFn = filterNonDiverseAlternatives(roadNetwork, batchingManager, theta) _
    val runFn    = runSelection(selectionRunner, roadNetwork, batchingManager, bank, zoneLookup) _

    logger.debug(s"batch $batchId running shortest path searches")
    val selectionRequestResult = for {
      reqWithPath <- RoutingOps.findShortestPathForBatch(search, requests)
      _ = logger.debug(s"shortest path search resulted in ${reqWithPath.length} paths")
      filtered <- reqWithPath.traverse(filterFn).map(_.flatten)
      _             = logger.debug(s"after filtering non-diverse pairs, ${filtered.length} paths")
      requestOption = asSelectionRequest(batchId, filtered)
      _             = logger.debug(s"selection request nonEmpty? ${requestOption.nonEmpty}")
      result <- requestOption.map(runFn).getOrElse(IO.pure(None))
      cnt = result.map { case (res, _) => res.responses.length }.getOrElse(0)
      _   = logger.debug(s"batch $batchId resolved with $cnt responses")
    } yield result

    selectionRequestResult
  }

  /**
    * filter cases where the current path and proposed path do not differ by
    * some theta value.
    *
    * @param rn
    * @param batchingManager
    * @param theta
    * @param tuple
    * @return
    */
  def filterNonDiverseAlternatives(
    rn: RoadNetworkIO,
    batchingManager: BatchingManager,
    theta: Double
  )(tuple: (Request, Path)): IO[Option[(Request, List[Path])]] = {
    val (req, newSpur) = tuple
    // we need to compare the provided faster path, starting/ending at the Request locations,
    // with just a spur with the same start location
    for {
      hist        <- IO.fromEither(batchingManager.storedHistory.getNewestDataOrError(req.agent))
      currentPath <- getCurrentPath(rn, hist)
      currentSpur = pathSpurFromCurrent(newSpur, currentPath)
      diverse <- sufficientlyDiverse(rn, currentPath, newSpur, theta)
      _ = logger.debug(s"request ${req.agent} has diverse paths? $diverse")
    } yield if (diverse) Some((req, List(newSpur, currentSpur))) else None
  }

  def asSelectionRequest(
    batchId: String,
    filteredRequests: List[(Request, List[Path])]
  ): Option[SelectionRunnerRequest] = {
    filteredRequests match {
      case reqs if reqs.length < 2 =>
        // logger.debug(s"batch $batchId does not have enough requests that are sufficiently diverse")
        None
      case reqs =>
        val msg = s"batch $batchId after generating new paths for agents, " +
          s"${filteredRequests.length} are sufficiently diverse"
        logger.debug(msg)
        Some(SelectionRunnerRequest(batchId, filteredRequests.toMap))
    }
  }

  def runSelection(
    runner: SelectionRunner,
    rn: RoadNetworkIO,
    bm: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]]
  )(request: SelectionRunnerRequest): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    import KarmaSelectionAlgorithmOps._
    import RoutingOps._
    val initFn = instantiateSelectionAlgorithm(runner, rn, bm, bank, zoneLookup) _
    for {
      initRunner      <- initFn(List(request))
      selectionOutput <- runSelectionWithBank(List(request), rn, initRunner, bank)
      (soResults, updatedBank) = selectionOutput
      oneResult <- extractSingularBatchResult(soResults)
    } yield {
      // at this point, the underlying KarmaSelectionAlgorithm should have applied
      // the NetworkPolicyFilter so that we only see UO route responses
      logger.debug(s"finished selection for batch ${request.batchId}")
      logger.debug(s"selection response non-empty? ${oneResult.nonEmpty}")
      oneResult
        .map { selectionResult =>
          val result = RoutingAlgorithm.Result(
            responses = selectionResult.selection.selectedRoutes,
            agentHistory = bm.storedHistory
          )
          (result, updatedBank)
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
    val currentEdges = current.map(_.edgeId).toSet

    val nonOverlapPctResult = proposed.filterNot(e => currentEdges.contains(e.edgeId)) match {
      case Nil => IO.pure(0.0)
      case nonOverlappingProposed =>
        for {
          proposedDist <- proposed.traverse(_.toEdgeData(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
          uniqueDist   <- nonOverlappingProposed.traverse(_.toEdgeData(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
        } yield if (proposedDist == 0.0) 0.0 else uniqueDist / proposedDist
    }

    nonOverlapPctResult.map(_ > theta)
  }
}
