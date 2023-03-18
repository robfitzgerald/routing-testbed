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
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, TraverseDirection}
import edu.colorado.fitzgero.sotestbed.algorithm.selection._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm._

final case class UserOptimalAuctionSelection(
  // batchingFunction: BatchingFunction,
  networkZoneBatching: NetworkZoneBatching[_],
  selectionRunner: SelectionRunner,
  // networkZones: Map[String, List[EdgeId]],
  theta: Double,
  replanAtSameLink: Boolean,
  useCurrentLinkFlows: Boolean
) extends RoutingAlgorithmV3
    with LazyLogging {

  import UserOptimalAuctionSelection._

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
    batchWindow: SimTime,
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])] = {

    val runnerAfterFix =
      KarmaSelectionAlgorithmOps.instantiateSelectionAlgorithm(
        selectionRunner,
        currentSimTime,
        batchWindow,
        roadNetwork,
        batchingManager,
        bank,
        networkZoneBatching.zones
      )

    // apply the batching strategy and then filter out batches based on some
    // basic rules like whether or not agents can replan at the same link as
    // a previous replanning
    val batchingResult =
      networkZoneBatching.updateBatchingStrategy(roadNetwork, requests, currentSimTime).map {
        case None => List.empty
        case Some(BatchingFunction.BatchingResult(routeRequests, _)) =>
          val preFiltered = RoutingOps.preFilterBatches(
            batchingManager = batchingManager,
            minBatchSize = 1,
            replanAtSameLink = replanAtSameLink,
            batches = routeRequests
          )
          preFiltered
      }

    val selectionResults =
      runnerAfterFix.flatMap { runner =>
        batchingResult.flatMap { batches =>
          // given a bank state, batch id, and list of requests for that batch, return either a
          // routing algorithm result and updated bank state, or None if no responses are to be sent +
          // the bank state stays the same.
          type ResolveFn =
            (Map[String, Karma], String, List[Request]) => IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]]
          val resolveFn: ResolveFn = resolveBatch(
            selectionRunner = runner,
            currentSimTime = currentSimTime,
            roadNetwork = roadNetwork,
            batchingManager = batchingManager,
            zoneLookup = networkZoneBatching.zones,
            theta = theta
          ) _

          val nBatches = batches.length
          logger.debug(s"running user optimal auction selection on $nBatches batches")

          // below we fold batches into this State record type using the Monad[IO].iterateUntilM method
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
    currentSimTime: SimTime,
    roadNetwork: RoadNetworkIO,
    batchingManager: BatchingManager,
    zoneLookup: Map[String, List[EdgeId]],
    theta: Double
  )(
    bank: Map[String, Karma],
    batchId: String,
    requests: List[Request]
  ): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    val pathFn = generatePathPair(roadNetwork, batchingManager, theta) _
    val runFn  = runSelection(selectionRunner, currentSimTime, roadNetwork, batchingManager, bank, zoneLookup) _

    logger.debug(s"batch $batchId running shortest path searches")
    val selectionRequestResult = for {
      reqsWithPaths <- requests.traverse(pathFn).map(_.flatten)
      _             = logger.debug(s"after generating diverse path pairs, ${reqsWithPaths.length} requests remain")
      requestOption = asSelectionRequest(batchId, reqsWithPaths)
      _             = logger.debug(s"selection request nonEmpty? ${requestOption.nonEmpty}")
      result <- requestOption.map(runFn).getOrElse(IO.pure(None))
      cnt = result.map { case (res, _) => res.responses.length }.getOrElse(0)
      _   = logger.debug(s"batch $batchId resolved with $cnt responses")
    } yield result

    selectionRequestResult
  }

  /**
    * generate the current path spur and, if it differs from the proposed spur by
    * some theta, return the pair with the request.
    *
    * @param rn
    * @param batchingManager
    * @param theta
    * @param tuple
    * @return
    */
  def generatePathPair(
    rn: RoadNetworkIO,
    batchingManager: BatchingManager,
    theta: Double
  )(request: Request): IO[Option[(Request, List[Path])]] = {
    val search: (EdgeId, EdgeId, TraverseDirection) => IO[Option[Path]] =
      DijkstraSearch.edgeOrientedShortestPath[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR](
        rn,
        (edge: EdgeBPR) => Cost(edge.observedTravelTime.value)
      )
    RoutingOps
      .findShortestPathForRequest(search, request)
      .flatMap {
        case None          => IO.pure(None)
        case Some(newSpur) =>
          // extract this agent's current path and strip to the current path spur with matching o/d to the $newSpur.
          //
          for {
            hist        <- IO.fromEither(batchingManager.storedHistory.getNewestDataOrError(request.agent))
            forkEdge    <- IO.fromOption(newSpur.headOption)(new Error(s"shortest path found is empty"))
            currentPath <- getCurrentPathWithCurrentCosts(rn, hist)
            currentSpur = currentPath.dropWhile(_.edgeId != forkEdge.edgeId)
            diverse <- sufficientlyDiverse(rn, currentPath, newSpur, theta)
            _ = logger.debug(s"request ${request.agent} has diverse paths? $diverse")
          } yield if (diverse) Some((request, List(newSpur, currentSpur))) else None
      }
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

  /**
    * runs the selection algorithm.
    * - for RL-based network policies, this first means calling the server with a GET ACTION
    *   request in order to generate network policy signals for each network zone
    *
    * @param runner
    * @param rn
    * @param bm
    * @param bank
    * @param zoneLookup
    * @param request
    * @return
    */
  def runSelection(
    runner: SelectionRunner,
    currentSimTime: SimTime,
    rn: RoadNetworkIO,
    bm: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]]
  )(request: SelectionRunnerRequest): IO[Option[(RoutingAlgorithm.Result, Map[String, Karma])]] = {
    import KarmaSelectionAlgorithmOps._
    import RoutingOps._
    for {
      selectionOutput <- runSelectionWithBank(List(request), currentSimTime, rn, runner, bank)
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
  def getCurrentPathWithCurrentCosts(rn: RoadNetworkIO, rrd: AgentBatchData.RouteRequestData): IO[Path] =
    rrd.route.traverse { _.toPathSegment.updateCostEstimate(rn) }

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
          proposedDist <- proposed.traverse(_.toEdgeDataButRetainCost(rn)).map(_.foldLeft(0.0)(_ + _.linkDistance))
          uniqueDist <- nonOverlappingProposed
            .traverse(_.toEdgeDataButRetainCost(rn))
            .map(_.foldLeft(0.0)(_ + _.linkDistance))
        } yield if (proposedDist == 0.0) 0.0 else uniqueDist / proposedDist
    }

    nonOverlapPctResult.map(_ > theta)
  }
}
