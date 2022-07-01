package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.{
  AltPathsAlgorithmResult,
  AltsResultData
}
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.BatchFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.{
  SelectionRunnerRequest,
  SelectionRunnerResult
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.{Karma, KarmaSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, RunTime, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment, RoadNetwork}

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
  altPathsAlgorithmRunner: AltPathsAlgorithmRunner,
  batchingFunction: BatchingFunction,
  batchFilterFunction: BatchFilterFunction,
  selectionRunner: SelectionRunner,
  minBatchSize: Int
) extends LazyLogging {

  val altsAlgName: String     = altPathsAlgorithmRunner.altPathsAlgorithm.getClass.getSimpleName
  val batchFilterName: String = batchFilterFunction.getClass.getSimpleName
  val selectionName: String   = selectionRunner.selectionAlgorithm.getClass.getSimpleName

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
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])] = {

    if (requests.isEmpty) {
      IO.pure((List.empty, bank))
    } else {
      logger.info("beginning SO routing algorithm")

      val currentPathFn: String => IO[Path] =
        RoutingAlgorithm2.getCurrentPath(batchingManager, roadNetwork, altPathsAlgorithmRunner.costFunction)

      val result: IO[IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])]] = for {
        batchingFunctionStartTime <- IO { System.currentTimeMillis }
        routeRequestsOpt          <- batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime)
        batchingRuntime           <- IO { RunTime(System.currentTimeMillis - batchingFunctionStartTime) }
      } yield {
        logger.info(s"request batching computed via ${batchingFunction.getClass.getSimpleName}")
        routeRequestsOpt match {
          case None =>
            logger.info("no viable requests after applying batching function")
            IO.pure((List.empty, bank))
          case Some(routeRequests) =>
            // remove route requests which we know do not meet the batch size requirements of our batch filter function
            val preFilteredRouteRequests = routeRequests.filter {
              case (_, reqs) => reqs.lengthCompare(minBatchSize) >= 0
            }

            // run alts path generator
            val altPathsResult: IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] =
              preFilteredRouteRequests.traverse {
                case (batchId, batch) =>
                  altPathsAlgorithmRunner.run(batchId, batch, batchingManager.storedHistory, roadNetwork)
              }

            // handle the special case of additional context required for running the karma algorithm
            // when working within the constraints of the SelectionAlgorithm trait
            def instantiateSelection(requests: List[SelectionRunnerRequest]) = {
              RoutingAlgorithm2.instantiateSelectionAlgorithm(
                selectionRunner,
                roadNetwork,
                requests,
                batchingManager,
                bank
              )
            }

            for {
              altsStartTime <- IO { System.currentTimeMillis }
              batchAlts     <- altPathsResult
              _             <- IO.pure(logger.info(s"alt paths computed via $altsAlgName"))
              altResultData = AltPathsAlgorithmRunner.logAltsResultData(batchAlts)
              altsRunTime             <- IO { RunTime(System.currentTimeMillis - altsStartTime) }
              batchAltsFiltered       <- batchFilterFunction.filter(batchAlts, roadNetwork)
              _                       <- IO.pure { logger.info(s"batch filter function completed via $batchFilterName") }
              selectionRunnerRequests <- RoutingAlgorithm2.getCurrentPaths(batchAltsFiltered, currentPathFn)
              selectionRunnerFixed    <- instantiateSelection(selectionRunnerRequests)
              selectionOutput <- RoutingAlgorithm2.runSelectionWithBank(
                selectionRunnerRequests,
                roadNetwork,
                selectionRunnerFixed,
                bank
              )
              (soResults, updatedBank) = selectionOutput
              //              soResults <- selectionRunnerRequests.traverse { r => selAlg.run(r, roadNetwork, bank) }
              _ <- IO.pure(logger.info(s"selection algorithm completed via $selectionName"))
            } yield {
              // re-combine data by batch id and package as a RoutingAlgorithm.Result
              val matched = RoutingAlgorithm2.matchAltBatchesWithSelectionBatches(
                batchAltsFiltered,
                soResults,
                batchingManager,
                altsRunTime,
                batchingRuntime,
                altResultData
              )
              (matched, updatedBank)
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
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner,
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner,
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
    * adds context to the selection algorithm specific to this time step.
    * this was created to deal with the Karma-based selection algorithm's requirements.
    * @param selectionRunner
    * @param roadNetwork
    * @param selectionRunnerRequest
    * @param batchingManager
    * @param bank
    * @return
    */
  def instantiateSelectionAlgorithm(
    selectionRunner: SelectionRunner,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    selectionRunnerRequest: List[SelectionRunnerRequest],
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[SelectionRunner] = {
    selectionRunner.selectionAlgorithm match {
      case k: KarmaSelectionAlgorithm =>
        // special handling for Karma-based selection
        // for each batch, observe the congestion effects of that batch
        val batchesWithCongestionObservations = selectionRunnerRequest
          .flatTraverse {
            case SelectionRunnerRequest(batchId, batch) =>
              val batchEdgeIds = batch.keys.toList.map { _.location }.distinct
              val observationIO = k.congestionObservation.observeCongestion(
                roadNetwork,
                k.freeFlowCostFunction,
                k.marginalCostFunction,
                batchEdgeIds
              )
              observationIO.map {
                case None      => List.empty
                case Some(obs) => List(batchId -> obs)
              }
          }

        // for each batch with observations, compute the network signal
        val batchesWithSignals = batchesWithCongestionObservations
          .map {
            _.map {
              case (batchId, obs) =>
                val signal = k.gen.generateSignal(obs)
                (batchId, obs, signal)
            }
          }

        // pass the generated batch data to the inner Karma algorithm
        batchesWithSignals.map { batches =>
          val observations = batches.map { case (bId, obs, _) => bId -> obs }.toMap
          val signals      = batches.map { case (bId, _, sig) => bId -> sig }.toMap
          val fixedKarmaSelection = k.build(
            batchingManager.storedHistory,
            observations,
            signals,
            k.selectionPw,
            k.networkPw
          )

          selectionRunner.copy(selectionAlgorithm = fixedKarmaSelection)
        }

      case _ => IO.pure(selectionRunner)
    }
  }

  /**
    * runs the selection algorithm for each selection request, holding aside the
    * effect of interacting with the bank between each run of the selection algorithm.
    *
    * @param requests sub-batch of request data for a selection algorithm
    * @param roadNetwork road network state
    * @param runner algorithm to apply
    * @param bank ledger containing current balances for all agents
    * @return the effect of running this time step of selection algorithms
    */
  def runSelectionWithBank(
    requests: List[SelectionRunnerRequest],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    runner: SelectionRunner,
    bank: Map[String, Karma]
  ): IO[(List[Option[SelectionRunnerResult]], Map[String, Karma])] = {
    val initial =
      IO.pure((List.empty[Option[SelectionRunnerResult]], bank))
    requests.foldLeft(initial) { (acc, r) =>
      acc.flatMap {
        case (results, innerBank) =>
          runner.run(r, roadNetwork, innerBank).map {
            case None                             => (None +: results, innerBank)
            case Some((result, updatedInnerBank)) => (Some(result) +: results, updatedInnerBank)
          }
      }
    }
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
      mostRecentOption <- IO.pure(batchingManager.storedHistory.getNewestData(agentId))
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
