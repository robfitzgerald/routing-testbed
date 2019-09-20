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
  qSim: QSim,
  controler: Controler,
  playPauseSimulationControl: PlayPauseSimulationControl,
  agentsInSimulationHandler: AgentsInSimulationNeedingReplanningHandler,
  roadNetworkDeltaHandler: RoadNetworkDeltaHandler,
  temporaryPathPrefixStore: collection.mutable.Map[Id[Person], List[Id[Link]]] = collection.mutable.Map.empty,
  replanningPayload: Option[MATSimProxy.RouteRequests] = None,
  currentSimTime: SimTime = SimTime.Zero
) {

  def hasCompletedDayAndExited(simulationTailTimeout: Duration = Duration(60, MINUTES))
    : Either[MATSimSimulation.IsDoneFailure, Boolean] = {
    if (playPauseSimulationControl.isFinished) Right(true)
    else {
      val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

      @tailrec
      def _isDone(): Either[MATSimSimulation.IsDoneFailure, Boolean] = {
        if (playPauseSimulationControl.isFinished) {
          Right(true)
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

  def advance(endOfRoutingTime: SimTime, stepSize: SimTime = SimTime(1)): MATSimSimulation = {

    // advance either the defined sim step, or, if we have reached the end of routing time,
    // then run the remaining simulation
    val thisSimStep: SimTime = if (endOfRoutingTime <= currentSimTime) {
      SimTime.EndOfDay
    } else stepSize

    // run simulation for the suggested duration
    playPauseSimulationControl.doStep(currentSimTime.value.toInt + thisSimStep.value.toInt)

    // update local time state
    this.copy(
      currentSimTime = currentSimTime + stepSize
    )
  }
}

object MATSimSimulation {
//  final class MutableSimulationState(
//      qSim: QSim,
//      controler: Controler,
//      agentsInSimulationHandler: AgentsInSimulationNeedingReplanningHandler,
//      roadNetworkDeltaHandler: RoadNetworkDeltaHandler,
//      temporaryPathPrefixStore: collection.mutable.Map[Id[Person], List[Id[Link]]] = collection.mutable.Map.empty,
//      replanningPayload: Option[MATSimProxy.RouteRequests] = None,
//      currentSimTime: SimTime = SimTime.Zero,
//      isRunning: Boolean = false,
//      isPaused: Boolean = false,
//      isDoneRouting: Boolean = false,
//      isDoneWithSimulation: Boolean = false
//  ) {
//
//    // register play/pause simulation control
//    private val playPauseSimulationControl: PlayPauseSimulationControl = new PlayPauseSimulationControl(qSim)
//    playPauseSimulationControl.pause()
//
//    def setSimTime(event: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {
//      currentSimTime = SimTime(event.getSimulationTime)
//    }
//    def getSimTime: SimTime = currentSimTime
//
//    def setIsRunning(): Unit = {
//      isRunning = true
//    }
//
//    def getIsRunning: Boolean = isRunning
//
//    def setIsPaused(): Unit = {
//
//      isPaused = true
//    }
//
//    def setIsNotPaused(): Unit = {
//      isPaused = false
//    }
//
//    def setIsDoneRouting(): Unit = {
//      isDoneRouting = true
//    }
//    def getIsDoneRouting: Boolean = isDoneRouting
//
//    def setIsDoneWithSimulation(): Unit = {
//      isDoneWithSimulation = true
//    }
//    def getIsDoneWithSimulation: Boolean = isDoneWithSimulation
//
//    def advance(stepSize: SimTime = SimTime(1)): MATSimSimulation = {
//      playPauseSimulationControl.doStep(currentSimTime.value.toInt + stepSize.value.toInt)
//
//    }
//  }
//
//  object MutableSimulationState {
//
//    def apply(
//        q: QSim,
//        c: Controler,
//        a: AgentsInSimulationNeedingReplanningHandler,
//        r: RoadNetworkDeltaHandler
//    ): MutableSimulationState = new MutableSimulationState(q, c, a, r)
//  }

  sealed trait IsDoneFailure

  object IsDoneFailure {
    final case class TimeoutFailure(message: String) extends IsDoneFailure
  }
}
