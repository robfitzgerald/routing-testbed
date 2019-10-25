package edu.colorado.fitzgero.sotestbed.matsim

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.LazyLogging
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
  currentSimTime: SimTime = SimTime.Zero,
  isRunning: Boolean = false
) extends Serializable with LazyLogging {

  def hasCompletedDayAndExited(
    simulationTailTimeout: Duration,
    playPauseSimulationControl: PlayPauseSimulationControl,
    thread: Thread
  ): Either[MATSimSimulation.IsDoneFailure, Boolean] = {

    val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

    @tailrec
    def _isDone(): Either[MATSimSimulation.IsDoneFailure, Boolean] = {
      if (!thread.isAlive) {
        Right(playPauseSimulationControl.isFinished)
      } else if (System.currentTimeMillis > timeoutStop) {
        Left(
          MATSimSimulation.IsDoneFailure.TimeoutFailure(
            s"surpassed timeout of ${simulationTailTimeout.toMinutes} minutes waiting for simulation to finish"))
      } else {
        Try { Thread.sleep(100) } match {
          case Success(()) => _isDone()
          case Failure(e) =>
            Left(MATSimSimulation.IsDoneFailure.TimeoutFailure(s"waiting for MATSim in child thread to terminate, failed: ${e.getStackTrace}"))
        }
      }
    }

    _isDone()
  }

  def advance(newTime: SimTime): MATSimSimulation =
    this.copy(
      currentSimTime = newTime
    )

//  def advance(endOfRoutingTime: SimTime, stepSize: SimTime = SimTime(1), playPauseSimulationControl: PlayPauseSimulationControl): MATSimSimulation = {
//
//    println()
//    // advance either the defined sim step, or, if we have reached the end of routing time,
//    // then run the remaining simulation
//    // guard against boundary conditions
//    val yoEndOfDay = SimTime.EndOfDay.value.toDouble
//    val getLocalYo = playPauseSimulationControl.getLocalTime
//    val advanceToSimTime: Int =
////      if (SimTime.EndOfDay.value.toDouble < playPauseSimulationControl.getLocalTime) 0
//    if (yoEndOfDay < getLocalYo) {
//      println()
//      0
//    } else {
//      val thisSimStep: SimTime = if (endOfRoutingTime <= currentSimTime) {
//        SimTime.EndOfDay
//      } else stepSize
//      currentSimTime.value.toInt + thisSimStep.value.toInt
//    }
//
//    // run simulation for the suggested duration
//    playPauseSimulationControl.synchronized {
//      playPauseSimulationControl.doStep(advanceToSimTime)
//    }
//    val newTime: Double = playPauseSimulationControl.getLocalTime
//
//    // update local time state
//    this.copy(
//      currentSimTime = SimTime(newTime)
//    )
//  }
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
