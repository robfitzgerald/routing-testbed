package edu.colorado.fitzgero.sotestbed.simulator

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

/**
  * a set of operations describing a simulator which is pausable and can be hand-cranked.
  * allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  * @tparam F a context for the computation (such as cats.effect.IO)
  */
trait SimulatorOps[F[_]] {
  type Simulator
  type SimulatorConfiguration

  /**
    * given a config, initializes the simulation in a paused state
    *
    * @param config configuration relevant to this simulator
    * @return the simulator state object
    */
  def initializeSimulator(config: SimulatorConfiguration): F[Simulator]

  /**
    * cranks the simulation forward
    *
    * @param simulator the simulator state object
    * @return simulator state after one crank
    */
  def advance(simulator: Simulator): F[Simulator]

  /**
    * captures the link flow deltas which occurred last time advance was called
    *
    * @param simulator the simulator state object
    * @return a list of edge id and marginal flow tuples
    */
  def getUpdatedEdges(simulator: Simulator): F[List[(EdgeId, Flow)]]

  /**
    * produces all routing requests related to the current time of the simulator
    *
    * @param simulator the simulator state object
    * @return a list of request objects translated into the routing framework
    */
  def getActiveRequests(simulator: Simulator): F[List[Request]]

  /**
    * takes routing responses and applies them to the associated agents in the simulation
    *
    * @param simulator the simulator state object
    * @param xs a list of responses
    * @return the simulator state object
    */
  def assignRoutes(simulator: Simulator, xs: List[Response]): F[Simulator]

  /**
    * returns one of the discrete simulation phases that this simulation is in
    *
    * @param simulator the simulator state object
    * @return either an error message, or, a [[SimulatorOps.SimulatorState]] object
    */
  def getState(simulator: Simulator): F[Either[String, SimulatorOps.SimulatorState]]

  /**
    * get a common time representation of the current simulation state
    *
    * @param simulator the simulator state object
    * @return a [[SimTime]] object representing time in seconds
    */
  def getCurrentSimTime(simulator: Simulator): F[SimTime]
}

object SimulatorOps {

  sealed trait SimulatorState
  object SimulatorState {
    final case object Uninitialized extends SimulatorState
    final case object Initialized   extends SimulatorState
    final case object Running       extends SimulatorState
    final case object Finishing     extends SimulatorState
    final case object Finished      extends SimulatorState
  }
}