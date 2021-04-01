package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.Monad
import cats.implicits._
import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.BatchFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, RunTime, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.{
  AltPathsAlgorithmResult,
  AltsResultData
}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  *
  * @param altPathsAlgorithmRunner an algorithm that computes alt paths for a batch
  * @param batchingFunction applies a means to group requests
  * @param batchFilterFunction removes batches based on a batch filtering heuristic
  * @param selectionRunner combinatorial search
  * @param minBatchSize ignore batches less than this size. this value should be a function of
  *                     the alt paths algorithm "k" and the batch filter function "minSearchSpaceSize" parameters
  */
case class RoutingAlgorithm2(
  altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, Coordinate, EdgeBPR],
  batchingFunction: BatchingFunction,
  batchFilterFunction: BatchFilterFunction,
  selectionRunner: SelectionRunner[Coordinate],
  minBatchSize: Int
) extends LazyLogging {

  /**
    * performs all steps related to solving SO route plans
    *
    * @param roadNetwork the current road network state
    * @param requests the requests at this time step
    * @param currentSimTime the current time
    * @param batchingManager tracks state of agent data and releases requests which can be routed
    * @return routing algorithm results for each batch
    */
  def runSO(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    requests: List[RouteRequestData],
    currentSimTime: SimTime,
    batchingManager: BatchingManager
  ): IO[List[(String, RoutingAlgorithm.Result)]] = {

    if (requests.isEmpty) {
      IO.pure(List.empty)
    } else {
      logger.info("beginning SO routing alorithm")

      val currentPathFn: String => IO[Path] =
        RoutingAlgorithm2.getCurrentPath(batchingManager, roadNetwork, altPathsAlgorithmRunner.costFunction)

      val result: IO[IO[List[(String, RoutingAlgorithm.Result)]]] = for {
        batchingFunctionStartTime <- IO { System.currentTimeMillis }
        routeRequestsOpt          <- batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime)
        batchingRuntime           <- IO { RunTime(System.currentTimeMillis - batchingFunctionStartTime) }
      } yield {
        logger.info(s"request batching computed via ${batchingFunction.getClass.getSimpleName}")
        routeRequestsOpt match {
          case None =>
            logger.info("no viable requests after applying batching function")
            IO.pure(List.empty)
          case Some(routeRequests) =>
            // remove route requests which we know do not meet the batch size requirements of our batch filter function
            val preFilteredRouteRequests = routeRequests.filter {
              case (_, reqs) => reqs.lengthCompare(minBatchSize) >= 0
            }

            val altPathsResult: IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] =
              preFilteredRouteRequests.traverse {
                case (batchId, batch) =>
                  for {
                    res <- altPathsAlgorithmRunner.run(batchId, batch, batchingManager.storedHistory, roadNetwork)
                  } yield res
              }

            for {
              altsStartTime <- IO { System.currentTimeMillis }
              batchAlts     <- altPathsResult
              _ <- IO.pure(
                logger.info(
                  s"alt paths computed via ${altPathsAlgorithmRunner.altPathsAlgorithm.getClass.getSimpleName}"
                )
              )
              altResultData = AltPathsAlgorithmRunner.logAltsResultData(batchAlts)
              altsRunTime <- IO { RunTime(System.currentTimeMillis - altsStartTime) }

              batchAltsFiltered <- batchFilterFunction.filter(batchAlts, roadNetwork)
              _ <- IO.pure {
                val bffName = batchFilterFunction.getClass.getSimpleName
                if (bffName.nonEmpty) {
                  logger.info(s"batch filter function completed via ${batchFilterFunction.getClass.getSimpleName}")
                }
              }
              selectionRunnerRequests <- RoutingAlgorithm2.getCurrentPaths(batchAltsFiltered, currentPathFn)
              soResults               <- selectionRunnerRequests.traverse { r => selectionRunner.run(r, roadNetwork) }
              _ <- IO.pure(
                logger.info(
                  s"selection algorithm completed via ${selectionRunner.selectionAlgorithm.getClass.getSimpleName}"
                )
              )
            } yield {
              // re-combine data by batch id and package as a RoutingAlgorithm.Result
              RoutingAlgorithm2.matchAltBatchesWithSelectionBatches(
                batchAltsFiltered,
                soResults,
                batchingManager,
                altsRunTime,
                batchingRuntime,
                altResultData
              )
            }
        }
      }

      result.flatten
    }

  }
}

object RoutingAlgorithm2 {

  /**
    * constructor which computes the minBatchSize
    *
    * @param altPathsAlgorithmRunner an algorithm that computes alt paths for a batch
    * @param batchingFunction applies a means to group requests
    * @param batchFilterFunction removes batches based on a batch filtering heuristic
    * @param selectionRunner combinatorial search
    * @param k number of alt paths as a parameter for the alt paths runner
    * @param minSearchSpaceSize ignore batches which cannot produce at least this many combinations
    * @return the Routing Algorithm, v2
    */
  def apply(
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, Coordinate, EdgeBPR],
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner[Coordinate],
    k: Int,
    minSearchSpaceSize: Int
  ): RoutingAlgorithm2 = {
    // find log of minSearchSpaceSize in the base of k
    val minBatchSize: Int = math.ceil(math.log(minSearchSpaceSize.toDouble) / math.log(k)).toInt
    RoutingAlgorithm2(
      altPathsAlgorithmRunner,
      batchingFunction,
      batchFilterFunction,
      selectionRunner,
      minBatchSize
    )
  }

  /**
    * helper function to gather all results back together as a routing result for a batch
    * @param alts all alt path results from the same time step
    * @param selections all selection results from the same time step
    * @param batchingManager the manager which provides lookup data for agent (short-term) histories
    * @return routing algorithm results which fit the requirements of the reporting code
    *         while allowing alts and selection results to be unordered and separated.
    */
  def matchAltBatchesWithSelectionBatches(
    alts: List[AltPathsAlgorithmResult],
    selections: List[Option[SelectionRunner.SelectionRunnerResult]],
    batchingManager: BatchingManager,
    alternatePathsRuntime: RunTime,
    batchingFunctionRuntime: RunTime,
    altsResultData: AltsResultData
  ): List[(String, RoutingAlgorithm.Result)] = {

    val altsLookup = alts.map { a => (a.batchId, a) }.toMap
    val result = for {
      selectionOpt    <- selections
      selectionResult <- selectionOpt
      batchId = selectionResult.batchId
      alts <- altsLookup.get(batchId)
    } yield {

      val routingResult = RoutingAlgorithm.Result(
        kspResult = alts.alts,
        filteredKspResult = alts.filteredAlts.getOrElse(alts.alts),
        responses = selectionResult.selection.selectedRoutes,
        agentHistory = batchingManager.storedHistory,
        kspRuntime = alternatePathsRuntime,
        batchingRuntime = batchingFunctionRuntime,
        selectionRuntime = selectionResult.runtime,
        altsResultData = altsResultData,
        selfishCost = selectionResult.selection.selfishCost,
        optimalCost = selectionResult.selection.estimatedCost,
        travelTimeDiff = selectionResult.selection.travelTimeDiff,
        meanTravelTimeDiff = selectionResult.selection.averageTravelTimeDiff,
        samples = selectionResult.selection.samples.value
      )
      (batchId, routingResult)
    }
    result
  }

  /**
    * grab the current path for this agentId, using the current road network state.
    *
    * @param batchingManager stores historical path data for active agents
    * @param roadNetwork the current road network state
    * @param costFunction edge cost function
    * @param agentId the agent id to find their current path
    * @return the requested path inside an effect type
    */
  def getCurrentPath(
    batchingManager: BatchingManager,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  )(agentId: String): IO[List[PathSegment]] = {
    val result = for {
      mostRecentOption <- IO.pure(batchingManager.storedHistory.getMostRecentDataFor(agentId))
    } yield {
      mostRecentOption match {
        case None =>
          IO.pure(List.empty[PathSegment])
        case Some(mostRecent) =>
          val inner = for {
            edges <- roadNetwork.edges(mostRecent.remainingRoute.map(_.edgeId))
          } yield {
            edges.map(e => PathSegment(e.edgeId, costFunction(e.attribute)))
          }
          inner
      }
    }
    result.flatten
  }

  /**
    * for all batches, construct SelectionRunner request objects
    *
    * @param batchAltsFiltered the final set of alt paths we are considering (for the final set of
    *                          batches we are considering)
    * @param currentPathFn a lookup function that gets the most recent path data for an agent
    * @return a list of selection runner requests wrapped in the effect of calling the road network
    */
  def getCurrentPaths(
    batchAltsFiltered: List[AltPathsAlgorithmResult],
    currentPathFn: String => IO[Path]
  ): IO[List[SelectionRunner.SelectionRunnerRequest]] = {
    batchAltsFiltered.traverse { b =>
      b.alts.toList
        .traverse { case (r, alts) => currentPathFn(r.agent).map { p => (r, p +: alts) } }
        .map { paths => SelectionRunner.SelectionRunnerRequest(b.batchId, paths.toMap) }
    }
  }
}
