package edu.colorado.fitzgero.sotestbed.experiment

import cats._
import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.AltPathsAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.BatchFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{
  ActiveAgentHistory,
  AgentBatchData,
  BatchingFunction,
  BatchingManager
}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, RoutingOps}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionRunner}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{Edge, EdgeBPR}
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator

abstract class RoutingExperiment2[V] extends Reports[IO, V, EdgeBPR] with HandCrankedSimulator[IO] {

  final case class ExperimentState(
    roadNetwork: RoadNetwork[IO, V, EdgeBPR],
    batchingManager: BatchingManager,
    simulatorState: HandCrankedSimulator.SimulatorState = HandCrankedSimulator.SimulatorState.Uninitialized
  )

  /**
    * run an experiment using a hand-cranked simulator and a set of algorithms
    *
    * @param config
    * @param roadNetwork
    * @param ueRoutingAlgorithm
    * @param updateFunction
    * @param batchingFunction
    * @param batchWindow
    * @param minBatchSize
    * @param minRequestUpdateThreshold
    * @param minNetworkUpdateThreshold
    * @param doneRoutingAtSimTime
    * @return
    */
  final def run(
    config: SimulatorConfiguration,
    roadNetwork: RoadNetwork[IO, V, EdgeBPR],
    ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, V, EdgeBPR]],
    updateFunction: Edge.UpdateFunction[EdgeBPR],
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, V, EdgeBPR],
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner[V],
    batchWindow: SimTime,
    minBatchSize: Int,
    minRequestUpdateThreshold: SimTime,
    minNetworkUpdateThreshold: SimTime,
    doneRoutingAtSimTime: SimTime
  ): IO[ExperimentState] = {

    def _run(startState: ExperimentState): IO[ExperimentState] = {

      val experiment: IO[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(r0, b0, _) =>
          for {
            _               <- advance() // should return updated simulator
            currentSimTime  <- getCurrentSimTime
            edges           <- getUpdatedEdges
            r1              <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            batchDataUpdate <- getAgentsNewlyAvailableForReplanning
            (ueRequests, soUpdate) = batchDataUpdate.partition { BatchingManager.splitUEFromSO }
            ueResults <- RoutingExperiment2.runUE(ueRoutingAlgorithm, ueRequests, r1)
            b1                  = b0.updateAgentBatchData(soUpdate)
            (b2, batchRequests) = b1.submitActiveRouteRequestsForReplanning(currentSimTime)

            soResults <- RoutingExperiment2.routeSOAgents(
              r1,
              batchRequests,
              currentSimTime,
              altPathsAlgorithmRunner,
              b2,
              batchingFunction,
              batchFilterFunction,
              selectionRunner
            )
            resolvedResults = BatchingManager.resolveRoutingResultBatches(List(ueResults) ::: soResults.map { _._2 })
            _ <- assignReplanningRoutes(resolvedResults)
            dataForReports = List(("ue", ueResults)) ::: soResults
            _  <- updateReports(dataForReports, r1, currentSimTime)
            s1 <- getState
          } yield {
            ExperimentState(r1, b2, s1)
          }
      } {
        case ExperimentState(_, _, s) =>
          // termination condition
          s == HandCrankedSimulator.SimulatorState.Finished
      }

      for {
        finalState <- experiment
      } yield {
        finishReports()
        finalState
      }
    }

    val simulationResult = for {
      _ <- initializeSimulator(config)
      initialExperimentState = ExperimentState(roadNetwork, BatchingManager(batchWindow, minRequestUpdateThreshold))
      result <- _run(initialExperimentState)
    } yield {
      result
    }

    simulationResult
  }
}

object RoutingExperiment2 {

  def runUE[V](
    ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, V, EdgeBPR]],
    ueRequests: List[AgentBatchData],
    roadNetwork: RoadNetwork[IO, V, EdgeBPR]
  ): IO[RoutingAlgorithm.Result] = {
    val ueRouteRequests: List[Request] = ueRequests.flatMap {
      case data: AgentBatchData.RouteRequestData =>
        data.request.requestClass match {
          case RequestClass.UE => Some { data.request }
          case _               => None
        }
      case _ => None
    }
    ueRoutingAlgorithm.map { _.route(ueRouteRequests, ActiveAgentHistory.NoHistory, roadNetwork) } match {
      case None =>
        IO.pure { RoutingAlgorithm.Result() }
      case Some(result) =>
        result
    }
  }

  /**
    * performs all steps related to solving SO route plans
    *
    * @param roadNetwork
    * @param requests
    * @param currentSimTime
    * @param altPathsAlgorithmRunner
    * @param batchingManager
    * @param batchingFunction
    * @param batchFilterFunction
    * @param selectionRunner
    * @tparam V
    * @return
    */
  def routeSOAgents[V](
    roadNetwork: RoadNetwork[IO, V, EdgeBPR],
    requests: List[RouteRequestData],
    currentSimTime: SimTime,
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner[IO, V, EdgeBPR],
    batchingManager: BatchingManager,
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner[V]
  ): IO[List[(String, RoutingAlgorithm.Result)]] = {
    val result = for {
      routeRequestsOpt <- batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime)
    } yield {
      routeRequestsOpt match {
        case None =>
          IO.pure(List.empty)
        case Some(routeRequests) =>
          val altPathsResult: IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] =
            routeRequests.traverse {
              case (batchId, batch) =>
                for {
                  res <- altPathsAlgorithmRunner.run(batchId, batch, batchingManager.storedHistory, roadNetwork)
                } yield res
            }
          for {
            batchAlts <- altPathsResult
            filteredAlts = batchFilterFunction.filter(batchAlts)
            soResults <- filteredAlts.traverse { r => selectionRunner.run(r.batchId, r.alts, roadNetwork) }
          } yield {
            // convert to RoutingAlgorithm.Result.. requires a Map[BatchId, T] for T = {AltsResult, SelectionResult}
            matchAltBatchesWithSelectionBatches(filteredAlts, soResults, batchingManager)
          }
      }
    }

    result.flatten
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
