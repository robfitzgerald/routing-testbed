package edu.colorado.fitzgero.sotestbed.experiment

import cats._

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.Edge
import edu.colorado.fitzgero.sotestbed.reports.Reports
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps

abstract class RoutingExperiment[F[_]: Monad, V, E] extends SimulatorOps[F] with Reports[F] {

  final case class ExperimentState(
    simulator: Simulator,
    roadNetwork: RoadNetwork[F, V, E],
    simulatorState: SimulatorOps.SimulatorState = SimulatorOps.SimulatorState.Uninitialized,
    error: Option[String] = None
  )

  final def run(
    config: SimulatorConfiguration,
    roadNetwork: RoadNetwork[F, V, E],
    routingAlgorithm: RoutingAlgorithm[F, V, E],
    updateFunction: Edge.UpdateFunction[E],
    doneRoutingAtSimTime: SimTime
  ): F[ExperimentState] = {

    def _run(startState: ExperimentState): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM {
        case ExperimentState(e0, r0, s0, _) =>
          for {
            e1             <- advance(e0) // should return updated simulator
            persons        <- getActiveRequests(e1)
            edges          <- getUpdatedEdges(e1)
            r1             <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
            result         <- routingAlgorithm.route(persons, r1)
            e2             <- assignRoutes(e1, result.responses) // should return updated simulator
            currentSimTime <- getCurrentSimTime(e2)
            _ = updateReports(result, currentSimTime) // unit is ok here, no modifications to application state
            simulatorState <- getState(e2)
          } yield {
            simulatorState match {
              case Right(newState) => ExperimentState(e2, r1, newState)
              case Left(error) => ExperimentState(e2, r1, s0, Some{error})
            }
          }
      }{ case ExperimentState(_, _, s, err) =>
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
      initialExperimentState = ExperimentState(initialSimulatorState, roadNetwork)
      result <- _run(initialExperimentState)
    } yield {
      result
    }
  }
}
