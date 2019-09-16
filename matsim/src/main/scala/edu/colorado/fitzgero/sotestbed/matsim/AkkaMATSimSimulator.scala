package edu.colorado.fitzgero.sotestbed.matsim

import scala.concurrent.duration.{Duration, MINUTES}

import cats.Id
import cats.effect.IO

import akka.actor.{Actor, ActorRef}
import akka.event.{Logging, LoggingAdapter}
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps

class AkkaMATSimSimulator(
    matsimActor: ActorRef,
    messageTimeout: Duration = Duration.apply(5, MINUTES)
) extends SimulatorOps[Id] with Actor {

  private val log: LoggingAdapter = Logging(context.system, this)

  type SimulatorConfiguration = MATSimConfig

  var isDoneStatus: Option[Boolean] = None

  def initializeSimulator(config: Option[MATSimConfig]): Id[Unit] = {

  }

  def advance(): Id[Unit] = ???

  def getUpdatedEdges: List[(EdgeId, Flow)] = ???

  def getActiveRequests: List[Request] = ???

  def assignRoutes(xs: List[Response]): Id[Unit] = ???

  def isDone: Id[Boolean] = {
    // request "Is Done" status
    matsimActor ! MATSimActor.Messages.IsDoneRequest

    // wait for reply
    val timeoutStop: Long = System.currentTimeMillis + messageTimeout.toMillis
    while (isDoneStatus.isEmpty && System.currentTimeMillis < timeoutStop) {
      wait(100)
    }

    //
    isDoneStatus match {
      case None =>
        log.error(s"isDone attempted but failed due to timeout")
        true
      case Some(isDoneResponse) =>
        isDoneResponse
    }
  }


  def getCurrentSimTime: Id[SimTime] = ???

  override def receive: Receive = {
    case MATSimActor.Messages.IsDoneResponse(isDone) =>
      isDoneStatus = Some(isDone)
  }
}
