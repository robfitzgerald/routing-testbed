package edu.colorado.fitzgero.sotestbed.simulator

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

trait SimulatorOps[F[_]] {
  type Simulator
  type SimulatorConfiguration
  def initializeSimulator(config: SimulatorConfiguration): F[Simulator]
  def advance(simulator: Simulator): F[Simulator]
  def getUpdatedEdges(simulator: Simulator): F[List[(EdgeId, Flow)]]
  def getActiveRequests(simulator: Simulator): F[List[Request]]
  def assignRoutes(simulator: Simulator, xs: List[Response]): F[Simulator]
  def isDone(simulator: Simulator): F[Boolean]
  def getCurrentSimTime(simulator: Simulator): F[SimTime]
}
