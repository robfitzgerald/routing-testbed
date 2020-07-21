package org.matsim.core.mobsim.framework

import java.util.concurrent.{Phaser, Semaphore}

import org.matsim.core.mobsim.framework.events.{MobsimAfterSimStepEvent, MobsimBeforeCleanupEvent, MobsimBeforeSimStepEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimAfterSimStepListener, MobsimBeforeCleanupListener, MobsimBeforeSimStepListener}

/**
  * an attempt to get around the leaky timing when using this approach for hand-cranking MATSim. turns out, it still can't fix the problem.
  *
  * based on org.matsim.core.mobsim.framework.PlayPauseSimulationControl in MATSim, but, allows selecting time steps less than a full second.
  * since aquiring this Semiphore has been found to be "leaky", we need a fraction of a second forward stepping
  * in order to "fall out" into just a one-second advancement.
  *
  * @param qSim
  */
class PlayPauseControlSecondStepper(val qSim: ObservableMobsim) {
  private val playPauseMobsimListener = new PlayPauseMobsimListener()
  qSim.addQueueSimulationListeners(playPauseMobsimListener)
  private var status: PlayPauseControlSecondStepper.Status.Value = PlayPauseControlSecondStepper.Status.PLAY
  final private val access                                       = new Semaphore(1, true)
  private var localTime: Double                                  = -1
  final private val stepDone                                     = new Phaser(1)

  private class PlayPauseMobsimListener extends MobsimBeforeSimStepListener with MobsimAfterSimStepListener with MobsimBeforeCleanupListener {
    override def notifyMobsimBeforeSimStep(event: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {
      try access.acquire()
      catch {
        case e: InterruptedException =>
          throw new RuntimeException(e)
      }
    }
    override def notifyMobsimAfterSimStep(event: MobsimAfterSimStepEvent[_ <: Mobsim]): Unit = {
      access.release()
      localTime = event.getSimulationTime
      // yy I am not so sure about the "int".  kai, nov'17
      stepDone.arriveAndAwaitAdvance
      // This is arrival by one party.  If "pause" has been pressed before, we have a second party, and thus do not
      // advance.
    }
    override def notifyMobsimBeforeCleanup(e: MobsimBeforeCleanupEvent[_ <: Mobsim]): Unit = {
      localTime = Double.MaxValue
      stepDone.arriveAndDeregister
    }
  }
  final def doStep(time: Double): Unit = {
    if (status eq PlayPauseControlSecondStepper.Status.PLAY) throw new IllegalStateException
    while ({ localTime < time }) {
      stepDone.arriveAndAwaitAdvance
      // as long as localTime < time, this acts as the second party, and thus the simulation progresses
      // otherwise, the doStep method will return.
    }
  }
  final def pause(): Unit = {
    if (status ne PlayPauseControlSecondStepper.Status.PAUSE) {
      stepDone.register
      // so when "pause" was hit, there is now a second party registered to the the phaser
      status = PlayPauseControlSecondStepper.Status.PAUSE
    }
  }
  final def play(): Unit = {
    if (status ne PlayPauseControlSecondStepper.Status.PLAY) {
      stepDone.arriveAndDeregister
      // the second party is de-registered, and thus it will now just play
      status = PlayPauseControlSecondStepper.Status.PLAY
    }
  }
  final def getAccess: Semaphore = access
  final def isFinished: Boolean  = localTime == Double.MaxValue
  final def getLocalTime: Double = localTime
}

object PlayPauseControlSecondStepper {

  object Status extends Enumeration {
    type Status = Value
    val PAUSE, PLAY = Value
  }
}
