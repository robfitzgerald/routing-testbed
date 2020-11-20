package edu.colorado.fitzgero.sotestbed.experiment

import cats._

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.{ActiveAgentHistory, AgentBatchData, BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.Edge
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator

abstract class RoutingExperiment[F[_]: Monad, V, E] extends Reports[F, V, E] with HandCrankedSimulator[F] {

  final case class ExperimentState(
    roadNetwork: RoadNetwork[F, V, E],
    batchingManager: BatchingManager,
    simulatorState: HandCrankedSimulator.SimulatorState = HandCrankedSimulator.SimulatorState.Uninitialized
  )

  /**
    * run an experiment using a hand-cranked simulator and a set of algorithms
    *
    * @param config
    * @param roadNetwork
    * @param ueRoutingAlgorithm
    * @param soRoutingAlgorithm
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
    roadNetwork: RoadNetwork[F, V, E],
    ueRoutingAlgorithm: Option[RoutingAlgorithm[F, V, E]],
    soRoutingAlgorithm: RoutingAlgorithm[F, V, E],
    updateFunction: Edge.UpdateFunction[E],
    batchingFunction: BatchingFunction,
    batchWindow: SimTime,
    minBatchSize: Int,
    minRequestUpdateThreshold: SimTime,
    minNetworkUpdateThreshold: SimTime,
    doneRoutingAtSimTime: SimTime,
  ): F[ExperimentState] = {

    def _run(startState: ExperimentState): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(r0, b0, s0) =>
          for {
            _               <- advance() // should return updated simulator
            currentSimTime  <- getCurrentSimTime
            edges           <- getUpdatedEdges
            r1              <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            batchDataUpdate <- getAgentsNewlyAvailableForReplanning
            (ueRequests, soUpdate) = batchDataUpdate.partition { BatchingManager.splitUEFromSO }
            ueResults <- RoutingExperiment.runUE(ueRoutingAlgorithm, ueRequests, r1)
            b1                  = b0.updateAgentBatchData(soUpdate)
            (b2, batchRequests) = b1.submitActiveRouteRequestsForReplanning(currentSimTime)
            routeRequestsOpt <- batchingFunction.updateBatchingStrategy(r1, batchRequests, currentSimTime)
            routeRequests = routeRequestsOpt.getOrElse(List.empty)
            soResults <- routeRequests.traverse {
              case (batchId, batch) =>
                soRoutingAlgorithm.route(batch, b2.storedHistory, r1).map { res =>
                  (batchId, res)
                }
            }
            resolvedResults = BatchingManager.resolveRoutingResultBatches(ueResults +: soResults.map { _._2 })
            _ <- assignReplanningRoutes(resolvedResults)
            dataForReports = ("ue", ueResults) +: soResults
            _              <- updateReports(dataForReports, r1, currentSimTime)
            simulatorState <- getState
          } yield {
            ExperimentState(r1, b2, simulatorState)
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

object RoutingExperiment {

  def runUE[F[_]: Monad, V, E](ueRoutingAlgorithm: Option[RoutingAlgorithm[F, V, E]],
                               ueRequests: List[AgentBatchData],
                               roadNetwork: RoadNetwork[F, V, E]): F[RoutingAlgorithm.Result] = {
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
        Monad[F].pure { RoutingAlgorithm.Result() }
      case Some(result) =>
        result
    }
  }
}
