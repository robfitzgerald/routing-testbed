package edu.colorado.fitzgero.sotestbed.experiment

import cats._

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.{AgentBatchData, BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.Edge
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps

abstract class RoutingExperiment[F[_]: Monad, V, E] extends SimulatorOps[F] with Reports[F] {

  final case class ExperimentState(
    simulator: Simulator,
    roadNetwork: RoadNetwork[F, V, E],
    batchingManager: BatchingManager,
    simulatorState: SimulatorOps.SimulatorState = SimulatorOps.SimulatorState.Uninitialized,
    error: Option[String] = None
  )

  final def run(
    config              : SimulatorConfiguration,
    roadNetwork         : RoadNetwork[F, V, E],
    ueRoutingAlgorithm  : Option[RoutingAlgorithm[F, V, E]],
    soRoutingAlgorithm  : RoutingAlgorithm[F, V, E],
    updateFunction      : Edge.UpdateFunction[E],
    batchingFunction    : BatchingFunction,
    batchWindow         : SimTime,
    minBatchSize        : Int,
    requestUpdateCycle  : SimTime,
    doneRoutingAtSimTime: SimTime,
    selfishOnly         : Boolean
  ): F[ExperimentState] = {

    def _run(startState: ExperimentState): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(e0, r0, b0, s0, _) =>
          for {
            e1                  <- advance(e0) // should return updated simulator
            currentSimTime      <- getCurrentSimTime(e1)
            edges               <- getUpdatedEdges(e1)
            r1                  <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            batchDataUpdate     <- getAgentsNewlyAvailableForReplanning(e1)
            (ueUpdate, soUpdate) = batchDataUpdate.partition { payload => selfishOnly || payload.request.requestClass == RequestClass.UE }
            ueResults           <- ueRoutingAlgorithm.map{_.route(ueUpdate.map{_.request}, r1)}.getOrElse(Monad[F].pure{RoutingAlgorithm.Result()})
            b1                   = b0.updateStoredBatchData(soUpdate, currentSimTime)
            batchStratUpdate    <- batchingFunction.updateBatchingStrategy(r1, soUpdate, b1.batchingStrategy, b1.agentBatchDataMap, currentSimTime)
            b2                   = b1.applyBatchingFunctionInstructions(batchStratUpdate, currentSimTime)
            (b3, batches)        = b2.getBatchesForTime(currentSimTime)
            soResults           <- batches.traverse { batch => soRoutingAlgorithm.route(batch, r1) }
            resolvedResults      = BatchingManager.resolveRoutingResultBatches(ueResults +: soResults)
            e2                  <- assignReplanningRoutes(e1, resolvedResults) // should return updated simulator
            _                    = updateReports(soResults, currentSimTime) // unit is ok here, no modifications to application state
            simulatorState      <- getState(e2)
          } yield {
            simulatorState match {
              case Right(newState) => ExperimentState(e2, r1, b3, newState)
              case Left(error)     => ExperimentState(e2, r1, b3, s0, Some { error })
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
