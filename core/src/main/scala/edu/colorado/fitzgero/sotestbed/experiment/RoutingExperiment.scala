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
      currentSimTime: SimTime = SimTime.Zero
  )

  final def run(
      config: SimulatorConfiguration,
      roadNetwork: RoadNetwork[F, V, E],
      routingAlgorithm: RoutingAlgorithm[F, V, E],
      updateFunction: Edge.UpdateFunction[E],
      doneRoutingAtSimTime: SimTime
  ): F[ExperimentState] = {

    def _run(startState: ExperimentState): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM { case ExperimentState(s0, r0, _) =>
        for {
          s1             <- advance(s0) // should return updated simulator
          edges          <- getUpdatedEdges(s1)
          r1             <- r0.updateEdgeFlows(edges, updateFunction) // should return updated road network
          persons        <- getActiveRequests(s1)
          result         <- routingAlgorithm.route(persons, r1)
          s2             <- assignRoutes(s1, result.responses) // should return updated simulator
          currentSimTime <- getCurrentSimTime(s2)
          _ = updateReports(result, currentSimTime) // unit is ok here, no modifications to application state
        } yield ExperimentState(s2, r1, currentSimTime) // should be passed updated versions of state
      }(_.currentSimTime == config)

      for {
        finalState <- experiment
      } yield {
        finishReports(finalState.simulator)
        finalState
      }
    }

    for {
      initialSimulatorState      <- initializeSimulator(config)
      initialExperimentState = ExperimentState(initialSimulatorState, roadNetwork)
      result <- _run(initialExperimentState)
    } yield {
      result
    }
  }
}
