package edu.colorado.fitzgero.sotestbed.simulator

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.model.agent.Response
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond

/**
  * a set of operations describing a simulator which is pausable and can be hand-cranked.
  * allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  * @tparam F a context for the computation (such as cats.effect.IO)
  */
trait HandCrankedSimulator[F[_]] {
  type SimulatorConfiguration

  /**
    * given a config, initializes the simulation in a paused state
    *
    * @param config configuration relevant to this simulator
    * @return the simulator state object
    */
  def initializeSimulator(config: SimulatorConfiguration): F[Unit]

  /**
    * cranks the simulation forward
    *
    * @return simulator state after one crank
    */
  def advance(): F[Unit]

  /**
    * captures the changes in state of the simulation
    *
    * @return a list of edge id, marginal flow, and optional observed speeds
    */
  def getUpdatedEdges: F[List[(EdgeId, Option[Flow], MetersPerSecond)]]

  /**
    * produces all routing requests which have recently become available for replanning
    * so that they may be considered by the BatchingManager for replanning assignment.
    *
    * @return a list of request objects translated into the routing framework
    */
  def getAgentsNewlyAvailableForReplanning: F[List[AgentBatchData]]

  /**
    * takes routing responses and applies them to the associated agents in the simulation
    *
    * @param xs a list of responses
    * @return the simulator state object
    */
  def assignReplanningRoutes(xs: List[Response]): F[Unit]

  /**
    * returns one of the discrete simulation phases that this simulation is in
    *
    * @return either an error message, or, a [[HandCrankedSimulator.SimulatorState]] object
    */
  def getState: F[HandCrankedSimulator.SimulatorState]

  /**
    * get a common time representation of the current simulation state
    *
    * @return a [[SimTime]] object representing time in seconds
    */
  def getCurrentSimTime: F[SimTime]
}

object HandCrankedSimulator {

  sealed trait SimulatorState

  object SimulatorState {
    final case object Uninitialized     extends SimulatorState
    final case object Initialized       extends SimulatorState
    final case object Running           extends SimulatorState
    final case object Finishing         extends SimulatorState
    final case object Finished          extends SimulatorState
    final case class Error(msg: String) extends SimulatorState
  }
}
