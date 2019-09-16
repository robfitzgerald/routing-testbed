package edu.colorado.fitzgero.sotestbed.experiment
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

trait Reports[F[_]] {
  type Simulator
  def updateReports(routingResult: RoutingAlgorithm.Result, currentSimTime: SimTime): F[Unit]
  def finishReports(simulator: Simulator): F[Unit]
}
