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
      startState: ExperimentState,
      routingAlgorithm: RoutingAlgorithm[F, V, E],
      updateFunction: Edge.UpdateFunction[E],
      doneRoutingAtSimTime: SimTime
  ): F[ExperimentState] = {

    def _run(): F[ExperimentState] = {

      val experiment: F[ExperimentState] = startState.iterateUntilM { state =>
        for {
          s1             <- advance(state.simulator) // should return updated simulator
          edges          <- getUpdatedEdges(s1)
          r1             <- state.roadNetwork.updateEdgeFlows(edges, updateFunction) // should return updated road network
          persons        <- getActiveRequests(s1)
          result         <- routingAlgorithm.route(persons, state.roadNetwork)
          s2             <- assignRoutes(s1, result.responses) // should return updated simulator
          currentSimTime <- getCurrentSimTime(state.simulator)
          _ = updateReports(result, currentSimTime) // unit is ok here, no modifications to application state
        } yield ExperimentState(s2, r1, currentSimTime) // should be passed updated versions of state
      }(_.currentSimTime == doneRoutingAtSimTime)

      for {
        finalState <- experiment
      } yield {
        finishReports(finalState.simulator)
        finalState
      }
    }

    for {
      _      <- initializeSimulator(config)
      result <- _run()
    } yield {
      result
    }
  }
}
