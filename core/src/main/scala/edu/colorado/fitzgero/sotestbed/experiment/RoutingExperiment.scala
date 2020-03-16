package edu.colorado.fitzgero.sotestbed.experiment

import cats._

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{ActiveAgentHistory, AgentBatchData, BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.Edge
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps

abstract class RoutingExperiment[F[_]: Monad, V, E] extends Reports[F, V, E] with SimulatorOps[F] {

  final case class ExperimentState(
    simulator: Simulator,
    roadNetwork: RoadNetwork[F, V, E],
    batchingManager: BatchingManager,
    simulatorState: SimulatorOps.SimulatorState = SimulatorOps.SimulatorState.Uninitialized,
    error: Option[String] = None
  )

  final def run(
    config: SimulatorConfiguration,
    roadNetwork: RoadNetwork[F, V, E],
    ueRoutingAlgorithm: Option[RoutingAlgorithm[F, V, E]],
    soRoutingAlgorithm: RoutingAlgorithm[F, V, E],
    updateFunction: Edge.UpdateFunction[E],
    batchingFunction: BatchingFunction,
    batchWindow: SimTime,
    minBatchSize: Int,
    requestUpdateCycle: SimTime,
    doneRoutingAtSimTime: SimTime,
    selfishOnly: Boolean
  ): F[ExperimentState] = {

    def _run(startState: ExperimentState): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(e0, r0, b0, s0, _) =>
          for {
            e1              <- advance(e0) // should return updated simulator
            currentSimTime  <- getCurrentSimTime(e1)
            edges           <- getUpdatedEdges(e1)
            r1              <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            batchDataUpdate <- getAgentsNewlyAvailableForReplanning(e1)
            (ueRequests, soUpdate) = batchDataUpdate.partition { payload =>
              selfishOnly || BatchingManager.splitUEFromSO(payload)
            }
            ueResults <- RoutingExperiment.runUE(ueRoutingAlgorithm, ueRequests, r1)
            b1                  = b0.updateAgentBatchData(soUpdate)
            (b2, batchRequests) = b1.submitActiveRouteRequestsForReplanning(currentSimTime)
            routeRequestsOpt <- batchingFunction.updateBatchingStrategy(r1, batchRequests, currentSimTime)
            routeRequests = routeRequestsOpt.getOrElse(List.empty)
            soResults <- routeRequests.traverse { batch =>
              soRoutingAlgorithm.route(batch, b2.storedHistory, r1)
            }
            resolvedResults = BatchingManager.resolveRoutingResultBatches(ueResults +: soResults)
            e2             <- assignReplanningRoutes(e1, resolvedResults) // should return updated simulator
            _              <- updateReports(soResults, r1, currentSimTime) // unit is ok here, no modifications to application state
            simulatorState <- getState(e2)
          } yield {
            simulatorState match {
              case Right(newState) => ExperimentState(e2, r1, b2, newState)
              case Left(error)     => ExperimentState(e2, r1, b2, s0, Some { error })
            }
          }
      } {
        case ExperimentState(_, _, _, s, err) =>
          // termination condition
          s == SimulatorOps.SimulatorState.Finished || err.isDefined
      }

      for {
        finalState <- experiment
      } yield {
        finishReports(finalState.simulator)
        finalState
      }
    }

    for {
      initialSimulatorState <- initializeSimulator(config)
      initialExperimentState = ExperimentState(initialSimulatorState, roadNetwork, BatchingManager(batchWindow, requestUpdateCycle, minBatchSize))
      result <- _run(initialExperimentState)
    } yield {
      result
    }
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
      case None         => Monad[F].pure { RoutingAlgorithm.Result() }
      case Some(result) => result
    }
  }
}
