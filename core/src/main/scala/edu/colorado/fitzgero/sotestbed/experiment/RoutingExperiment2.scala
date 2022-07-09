package edu.colorado.fitzgero.sotestbed.experiment

import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
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
import edu.colorado.fitzgero.sotestbed.algorithm.routing.{RoutingAlgorithm, RoutingAlgorithm2}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.{Karma, KarmaSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.{Edge, EdgeBPR}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost

abstract class RoutingExperiment2
    extends Reports[IO, Coordinate, EdgeBPR]
    with HandCrankedSimulator[IO]
    with LazyLogging {

  final case class ExperimentState(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    batchingManager: BatchingManager,
    bank: Map[String, Karma],
    simulatorState: HandCrankedSimulator.SimulatorState = HandCrankedSimulator.SimulatorState.Uninitialized
  )

  /**
    * run an experiment using a hand-cranked simulator and a set of algorithms
    */
  final def run(
    config: SimulatorConfiguration,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, Coordinate, EdgeBPR]],
    updateFunction: Edge.UpdateFunction[EdgeBPR],
    costFunction: EdgeBPR => Cost,
    soRoutingAlgorithm: Option[RoutingAlgorithm2],
    bank: Map[String, Karma],
    batchWindow: SimTime,
    minRequestUpdateThreshold: SimTime
  ): IO[ExperimentState] = {

    def _run(startState: ExperimentState): IO[ExperimentState] = {

      val experiment: IO[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(r0, b0, k0, _) =>
          for {
            _              <- advance() // should return updated simulator
            currentSimTime <- getCurrentSimTime
            _ = logger.info(s"current simulation time: $currentSimTime")
            edges           <- getUpdatedEdges
            r1              <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            batchDataUpdate <- getAgentsNewlyAvailableForReplanning
            (ueRequests, soUpdate) = batchDataUpdate.partition { BatchingManager.splitUEFromSO }
            ueResults <- RoutingExperiment2.runUE(ueRoutingAlgorithm, ueRequests, r1)
            b1        <- b0.updateAgentBatchData(soUpdate, r1)
            (b2, batchRequests) = b1.submitActiveRouteRequestsForReplanning(currentSimTime)
            soOutput <- soRoutingAlgorithm
              .map { _.runSO(r1, batchRequests, currentSimTime, b2, k0) }
              .getOrElse(IO.pure((List.empty, k0)))
            (soResults, k1) = soOutput
            resolvedResults = BatchingManager.resolveRoutingResultBatches(List(ueResults) ::: soResults.map { _._2 })
            _ <- assignReplanningRoutes(resolvedResults)
            dataForReports = List(("ue", ueResults)) ::: soResults
            _  <- updateReports(dataForReports, r1, currentSimTime)
            s1 <- getState
          } yield {
            ExperimentState(r1, b2, k1, s1)
          }
      } {
        case ExperimentState(_, _, _, s) =>
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

    val initialState = ExperimentState(
      roadNetwork,
      BatchingManager(batchWindow, minRequestUpdateThreshold, costFunction),
      bank
    )
    val simulationResult = for {
      _      <- initializeSimulator(config)
      result <- _run(initialState)
    } yield {
      // handle closing the PrintWriter in the Karma selection algorithm (sigh)
      soRoutingAlgorithm.foreach { RoutingExperiment2.close }
      result
    }

    simulationResult
  }
}

object RoutingExperiment2 {

  /**
    * if provided, run a UE routing algorithm against the set of UE requests
    *
    * @param ueRoutingAlgorithm a routing algorithm. if not provided, no requests are routed with a UE objective
    * @param agentBatchData the new routing requests arriving from the simulator
    * @param roadNetwork the current network state
    * @tparam V vertex type
    * @return the result of this routing algorithm
    */
  def runUE[V](
    ueRoutingAlgorithm: Option[RoutingAlgorithm[IO, V, EdgeBPR]],
    agentBatchData: List[AgentBatchData],
    roadNetwork: RoadNetwork[IO, V, EdgeBPR]
  ): IO[RoutingAlgorithm.Result] = {

    val ueRequests: List[Request] = agentBatchData.collect {
      case data: AgentBatchData.RouteRequestData =>
        data.request.requestClass match {
          case RequestClass.UE => data.request
          case _               => throw new IllegalStateException("shouldn't be running UE on SO agents")
        }
    }
    val result: IO[RoutingAlgorithm.Result] =
      ueRoutingAlgorithm.map { _.route(ueRequests, ActiveAgentHistory.NoHistory, roadNetwork) } match {
        case None =>
          IO.pure { RoutingAlgorithm.Result() }
        case Some(routingResult) =>
          routingResult
      }
    result
  }

  def close(alg: RoutingAlgorithm2): Unit = {
    alg.selectionRunner.selectionAlgorithm match {
      case k: KarmaSelectionAlgorithm => k.close()
      case _                          => ()
    }
  }
}
