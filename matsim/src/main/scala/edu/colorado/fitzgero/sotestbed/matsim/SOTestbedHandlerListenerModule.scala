//package edu.colorado.fitzgero.sotestbed.matsim
//
//import java.time.LocalTime
//
//import language.postfixOps
//
//import akka.actor.{ Actor, ActorRef, ActorSystem, PoisonPill, Props }
//import akka.actor.ActorRef
//import akka.event.LoggingAdapter
//import edu.colorado.fitzgero.sotestbed.matsim.SOTestbedHandlerListenerModule.ReplanningPayload
//import org.matsim.core.controler.{AbstractModule, Controler}
//import org.matsim.core.mobsim.framework.Mobsim
//import org.matsim.core.mobsim.framework.events.{MobsimBeforeCleanupEvent, MobsimBeforeSimStepEvent}
//import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeCleanupListener, MobsimBeforeSimStepListener}
//import org.matsim.core.mobsim.qsim.{QSim, QSimUtils}
//import org.matsim.core.replanning.strategies.DefaultPlanStrategiesModule.DefaultSelector
//import org.matsim.core.replanning.strategies.SelectBest
//import org.matsim.withinday.controller.WithinDayModule
//
///**
//  * interrupts MATSim at the start of each time step and passes control back to the MATSimProxy, which in turn asks the route planner to solve a routing problem
//  * @param matsimProxyActor
//  * @param parentClass
//  * @param log
//  */
//class SOTestbedHandlerListenerModule(
//    matsimProxyActor: ActorRef,
//    controler: Controler,
//    parentClass: MATSimActor,
//    log: LoggingAdapter
//) extends AbstractModule {
//
//  def install(): Unit = {
//    log.info(
//      "installing overriding handler/listener SOTestbedHandlerListenerModule in MATSim simulator")
//
//    val qSim: QSim = QSimUtils.createDefaultQSim(controler.getScenario, controler.getEvents)
//
//    qSim.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
//      override def notifyMobsimBeforeSimStep(e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {
//        // parentClass ! goCalculateThings
//        val timeOfDay = qSim.getSimTimer.getTimeOfDay
//        val replanningPayload: SOTestbedHandlerListenerModule.ReplanningPayload =
//          ReplanningPayload(
//            timeOfDay
//          )
//        parentClass() ! replanningPayload
//        qSim.wait()
//      }
//    })
//
//    qSim.addQueueSimulationListeners(new MobsimBeforeCleanupListener {
//      override def notifyMobsimBeforeCleanup(e: MobsimBeforeCleanupEvent[_ <: Mobsim]): Unit = {
//        val timeOfDay = qSim.getSimTimer.getTimeOfDay
//        val safeTime =
//          if (timeOfDay > LocalTime.MAX.toSecondOfDay)
//            s"${(timeOfDay - LocalTime.MAX.toSecondOfDay) / 60D} minutes after end of day"
//          else s"${LocalTime.ofSecondOfDay(timeOfDay.toLong)}"
//        log.info(s"ending MATSim simulation at $safeTime")
//      }
//    })
//  }
//
//}
//
//object SOTestbedHandlerListenerModule {
//
//  def apply(
//    matsimProxyActor: ActorRef,
//    controler: Controler,
//    parentClass: MATSimActor,
//    log: LoggingAdapter
//  ): SOTestbedHandlerListenerModule =
//    new SOTestbedHandlerListenerModule(matsimProxyActor, controler, parentClass, log)
//
//  case class ReplanningPayload(
//    timeOfDay: Double
//  )
//}
