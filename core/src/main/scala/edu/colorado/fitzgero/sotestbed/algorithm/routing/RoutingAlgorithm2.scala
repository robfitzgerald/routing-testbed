package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.BatchFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult

/**
  *
  * @param altPathsAlgorithmRunner an algorithm that computes alt paths for a batch
  * @param batchingFunction applies a means to group requests
  * @param batchFilterFunction removes batches based on a batch filtering heuristic
  * @param selectionRunner combinatorial search
  * @param minBatchSize ignore batches less than this size. this value should be a function of
  *                     the alt paths algorithm "k" and the batch filter function "minSearchSpaceSize" parameters
  * @tparam V coordinate type
  */
case class RoutingAlgorithm2[V](
  altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, V, EdgeBPR],
  batchingFunction: BatchingFunction,
  batchFilterFunction: BatchFilterFunction,
  selectionRunner: SelectionRunner[V],
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
    roadNetwork: RoadNetwork[IO, V, EdgeBPR],
    requests: List[RouteRequestData],
    currentSimTime: SimTime,
    batchingManager: BatchingManager
  ): IO[List[(String, RoutingAlgorithm.Result)]] = {

    if (requests.isEmpty) {
      IO.pure(List.empty)
    } else {
      logger.info("beginning SO routing alorithm")

      val result: IO[IO[List[(String, RoutingAlgorithm.Result)]]] = for {
        routeRequestsOpt <- batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime)
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
              batchAlts <- altPathsResult
              _ <- IO.pure(
                logger.info(
                  s"alt paths computed via ${altPathsAlgorithmRunner.altPathsAlgorithm.getClass.getSimpleName}"
                )
              )
              batchAltsFiltered = batchFilterFunction.filter(batchAlts)
              _ <- IO.pure(
                logger.info(s"batch filter function completed via ${batchFilterFunction.getClass.getSimpleName}")
              )
              soResults <- batchAltsFiltered.traverse { r => selectionRunner.run(r.batchId, r.alts, roadNetwork) }
              _ <- IO.pure(
                logger.info(
                  s"selection algorithm completed via ${selectionRunner.selectionAlgorithm.getClass.getSimpleName}"
                )
              )
            } yield {
              // re-combine data by batch id and package as a RoutingAlgorithm.Result
              RoutingAlgorithm2.matchAltBatchesWithSelectionBatches(batchAltsFiltered, soResults, batchingManager)
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
    * @tparam V coordinate type
    * @return the Routing Algorithm, v2
    */
  def apply[V](
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, V, EdgeBPR],
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner[V],
    k: Int,
    minSearchSpaceSize: Int
  ): RoutingAlgorithm2[V] = {
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
    batchingManager: BatchingManager
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
        kspRuntime = alts.runtimeMilliseconds,
        selectionRuntime = selectionResult.runtime,
        travelTimeDiff = selectionResult.selection.travelTimeDiff,
        meanTravelTimeDiff = selectionResult.selection.averageTravelTimeDiff,
        samples = selectionResult.selection.samples.value
      )
      (batchId, routingResult)
    }
    result
  }
}
