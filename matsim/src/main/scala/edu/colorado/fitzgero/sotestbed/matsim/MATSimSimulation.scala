package edu.colorado.fitzgero.sotestbed.matsim

import scala.annotation.tailrec
import scala.concurrent.duration._

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.Event
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.Person
import org.matsim.core.controler.Controler
import org.matsim.core.mobsim.framework.{Mobsim, PlayPauseSimulationControl}
import org.matsim.core.mobsim.framework.events.MobsimBeforeSimStepEvent
import org.matsim.core.mobsim.qsim.QSim

final case class MATSimSimulation(
    mutableSimulationState: MATSimSimulation.MutableSimulationState
) {

  def isDone(simulationTailTimeout: Duration = Duration(60, MINUTES))
    : Either[MATSimSimulation.IsDoneFailure, Boolean] = {
    if (mutableSimulationState.getIsDoneWithSimulation) Right(true)
    else {
      val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

      @tailrec
      def _isDone(): Either[MATSimSimulation.IsDoneFailure, Boolean] = {
        if (mutableSimulationState.getIsDoneWithSimulation) {
          Right(true)
        } else if (!mutableSimulationState.getIsDoneRouting) {
          Right(false)
        } else if (System.currentTimeMillis > timeoutStop) {
          Left(MATSimSimulation.IsDoneFailure.TimeoutFailure(
            s"surpassed timeout of ${simulationTailTimeout.toMinutes} minutes waiting for simulation to finish"))
        } else {
          wait(Duration(5, SECONDS).toMillis)
          _isDone()
        }
      }

      _isDone()
    }
  }
}

object MATSimSimulation {
  final class MutableSimulationState(
      val qSim: QSim,
      val controler: Controler,
      val agentsInSimulationHandler: AgentsInSimulationNeedingReplanningHandler,
      val roadNetworkDeltaHandler: RoadNetworkDeltaHandler,
      val temporaryPathPrefixStore: collection.mutable.Map[Id[Person], List[Id[Link]]] = collection.mutable.Map.empty,
      var replanningPayload: Option[MATSimProxy.RouteRequests] = None,
      private var currentSimTime: SimTime = SimTime.Zero,
      private var isRunning: Boolean = false,
      private var isPaused: Boolean = false,
      private var isDoneRouting: Boolean = false,
      private var isDoneWithSimulation: Boolean = false
  ) {

    // register play/pause simulation control
    private val playPauseSimulationControl: PlayPauseSimulationControl = new PlayPauseSimulationControl(qSim)
    playPauseSimulationControl.pause()

    def setSimTime(event: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {
      currentSimTime = SimTime(event.getSimulationTime)
    }
    def getSimTime: SimTime = currentSimTime

    def setIsRunning(): Unit = {
      isRunning = true
    }

    def getIsRunning: Boolean = isRunning

    def setIsPaused(): Unit = {

      isPaused = true
    }

    def setIsNotPaused(): Unit = {
      isPaused = false
    }

    def setIsDoneRouting(): Unit = {
      isDoneRouting = true
    }
    def getIsDoneRouting: Boolean = isDoneRouting

    def setIsDoneWithSimulation(): Unit = {
      isDoneWithSimulation = true
    }
    def getIsDoneWithSimulation: Boolean = isDoneWithSimulation

    def advance(stepSize: SimTime = SimTime(1)): MATSimSimulation = {
      playPauseSimulationControl.doStep(currentSimTime.value.toInt + stepSize.value.toInt)

    }
  }

  object MutableSimulationState {

    def apply(
        q: QSim,
        c: Controler,
        a: AgentsInSimulationNeedingReplanningHandler,
        r: RoadNetworkDeltaHandler
    ): MutableSimulationState = new MutableSimulationState(q, c, a, r)
  }

  sealed trait IsDoneFailure

  object IsDoneFailure {
    final case class TimeoutFailure(message: String) extends IsDoneFailure
  }
}
