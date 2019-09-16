package edu.colorado.fitzgero.sotestbed.matsim

import java.nio.file.{Files, Path}
import java.time.LocalTime

import scala.collection.JavaConverters._
import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef}
import akka.event.{Logging, LoggingAdapter}
import akka.util.Timeout
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, MetersPerSecond, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.{Activity, Leg, Person}
import org.matsim.core.config._
import org.matsim.core.controler.{AbstractModule, Controler}
import org.matsim.core.mobsim.framework.events.{MobsimBeforeCleanupEvent, MobsimBeforeSimStepEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeCleanupListener, MobsimBeforeSimStepListener}
import org.matsim.core.mobsim.framework.{Mobsim, MobsimAgent}
import org.matsim.core.mobsim.qsim.{QSim, QSimUtils}
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.NetworkRoute
import org.matsim.core.replanning.PlanStrategy

class MATSimActor(
    routingActor: ActorRef,
    messageTimeout: Duration = Duration.apply(5, MINUTES)
) extends Actor {

  private val matsimConfig: Config                                    = ConfigUtils.createConfig()
  private[matsim] var matsimSimulationState: Option[MATSimSimulation] = None
  var isPaused: Boolean = false
  var isDoneRouting: Boolean = false
  var isDoneWithSimulation: Boolean = false

  // if we are requesting re-planning from an edge we are traversing in the future, then we can store the
  // path leading up to it here
  private[MATSimActor] val temporaryPathPrefixStore
    : collection.mutable.Map[Id[Person], List[Id[Link]]] = collection.mutable.Map.empty
  private val log: LoggingAdapter                        = Logging(context.system, this)

  def receive: Receive = {
    ///////////////////////////////////////////////// load and begin the simulation
    case conf: MATSimConfig =>
      // file system configuration
      val experimentPath: Path =
        conf.fs.workingDirectory.resolve(conf.fs.experimentSubdirectoryName)
      Files.createDirectories(experimentPath)
      matsimConfig.controler().setOutputDirectory(experimentPath.toString)

      // matsim run configuration
      matsimConfig.controler.setLastIteration(conf.run.iterations)

      // start MATSim and capture object references to simulation in broader MATSimActor scope
      matsimSimulationState = Some {

        val controler: Controler = new Controler(matsimConfig)
        val qSim                 = QSimUtils.createDefaultQSim(controler.getScenario, controler.getEvents)
        val agentsInSimulationHandler: AgentsInSimulationNeedingReplanningHandler =
          new AgentsInSimulationNeedingReplanningHandler(conf.routing.agentsUnderControl)
        val roadNetworkDeltaHandler: RoadNetworkDeltaHandler = new RoadNetworkDeltaHandler()

        controler.addOverridingModule(new AbstractModule {
          def install(): Unit = {
            log.info("installing overriding handler/listener in MATSim simulator")

            qSim.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
              override def notifyMobsimBeforeSimStep(
                  e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {

                if (isDoneRouting) {
                  log.info(s"SimTime ${e.getSimulationTime} is beyond end of routing time ${conf.run.endOfRoutingTime} set in config - no routing will occur")
                } else {

                  // get the changes to the road network observed since last sim step
                  val networkDeltas: Map[EdgeId, Int] = roadNetworkDeltaHandler.getDeltasAsEdgeIds
                  roadNetworkDeltaHandler.clear()

                  // find the agents who are eligible for re-planning
                  val agentsInSimulation: Map[Id[Person], MobsimAgent] =
                    e.getQueueSimulation.asInstanceOf[QSim].getAgents.asScala.toMap
                  val agentsEligibleForReplanning: List[MobsimAgent] =
                    agentsInSimulationHandler.getActiveAgentIds
                      .foldLeft(List.empty[MobsimAgent]) { (agents, agentInSimulation) =>
                        agentsInSimulation.get(agentInSimulation) match {
                          case None =>
                            throw new IllegalStateException(
                              s"agent $agentInSimulation that emitted a departure event was not found in QSim")
                          case Some(agent) => agent +: agents
                        }
                      }

                  // convert the active agents eligible for re-planning into a List of Requests
                  val agentsForReplanning: List[Request] =
                    agentsEligibleForReplanning
                      .flatMap {
                        mobsimAgent: MobsimAgent =>
                          // TODO: turn into a route request. we want to
                          //   1. see if agentsInSimulationHandler has a path, and, if so,
                          //     try and estimate a good point to modify the agent's remaining path
                          //     or just pass the current point they are at if they are new to the routing algorithm
                          //   2. pass this origin, and their known destination (mobsimAgent.getDestinationLinkId) to the route planner

                          // MISSING: a way to estimate travel time over the road network links
                          val agentId: Id[Person]     = mobsimAgent.getId
                          val currentEdgeId: Id[Link] = mobsimAgent.getCurrentLinkId

                          // we want the remaining previously-assigned route starting from the agent's current Id[Link]
                          val previousPathFromCurrentOrigin: List[Id[Link]] =
                            agentsInSimulationHandler.getPathForAgent(agentId) match {
                              case None               => List.empty
                              case Some(previousPath) =>
                                // catch up with recent history. this can be empty if the current EdgeId isn't on the recorded path
                                previousPath.dropWhile(_ != currentEdgeId).map { Id.createLinkId(_) }
                            }

                          // the easy part...
                          val requestDestination: EdgeId =
                            EdgeId(mobsimAgent.getDestinationLinkId.toString)

                          // pick a reasonable starting point.
                          // we want to find a point that will take at least "conf.routing.reasonableReplanningLeadTime" to reach
                          // which we compute based on the free flow travel time for the remaining links.
                          val requestOriginOption: Option[EdgeId] =
                          previousPathFromCurrentOrigin match {
                            case Nil =>
                              // no previously assigned route, so, start from currentEdgeId
                              Some { EdgeId(currentEdgeId.toString) }
                            case previousPath =>
                              val reasonableStartPointFoldAccumulator
                              : MATSimActor.ReasonableStartPointFoldAccumulator = previousPath
                                .map { l =>
                                  val link: Link = qSim.getNetsimNetwork
                                    .getNetsimLink(Id.createLinkId(l.toString))
                                    .getLink
                                  (l, Meters(link.getLength) / MetersPerSecond(link.getFreespeed))
                                }
                                .foldLeft(MATSimActor.ReasonableStartPointFoldAccumulator(
                                  conf.routing.reasonableReplanningLeadTime)) {
                                  (acc, tup) =>
                                    acc.startPoint match {
                                      case Some(_) =>
                                        // already found - skip
                                        acc
                                      case None =>
                                        val (linkId, travelTimeSeconds) = tup
                                        val nextRemainingSlack
                                        : TravelTimeSeconds = acc.remainingSlack - travelTimeSeconds
                                        if (nextRemainingSlack <= TravelTimeSeconds.Zero)
                                          acc.copy(
                                            remainingSlack = nextRemainingSlack,
                                            startPoint = Some { EdgeId(linkId.toString) }
                                          )
                                        else
                                          acc.copy(
                                            remainingSlack = nextRemainingSlack,
                                            pathPrefix = acc.pathPrefix :+ linkId
                                          )
                                    }
                                }

                              // if there isn't a reasonable future replanning point, then we shouldn't disturb this agent

                              reasonableStartPointFoldAccumulator.startPoint match {
                                case None =>
                                  agentsInSimulationHandler.getNumberOfPathAssignmentsForAgent(
                                    agentId) match {
                                    case None =>
                                      throw new IllegalStateException(
                                        s"agent $agentId that emmitted a departure event should be stored in our handler")
                                    case Some(numRoutes) =>
                                      if (numRoutes == 0) {
                                        // ok. agent's trip is short, we still need to find a route, even though
                                        // a route would be shorter than the lower bound of "conf.routing.reasonableReplanningLeadTime"
                                        temporaryPathPrefixStore.update(agentId, List.empty)
                                        Some { EdgeId(currentEdgeId.toString) }
                                      } else {
                                        // ok, this agent's remaining trip is too short as it is below the lower bound
                                        // provided by "conf.routing.reasonableReplanningLeadTime",
                                        // so we won't send a re-routing request here
                                        None
                                      }
                                  }
                                case Some(startPoint) =>
                                  temporaryPathPrefixStore
                                    .update(agentId, reasonableStartPointFoldAccumulator.pathPrefix)
                                  Some { startPoint }
                              }
                          }

                          // requestOriginOption, if none, means we are cancelling this agent's route request
                          // which is our way of telling the algorithm that the agent's remaining trip is too
                          // short to re-plan
                          requestOriginOption.map { requestOrigin =>
                            Request(agentId.toString,
                              requestOrigin,
                              requestDestination,
                              RequestClass.SO(),
                              mobsimAgent.getMode)
                          }
                      }

                  // construct the payload for this routing request
                  val replanningPayload: MATSimActor.Messages.RouteRequests =
                    MATSimActor.Messages.RouteRequests(
                      e.getSimulationTime,
                      agentsForReplanning,
                      networkDeltas
                    )

                  // send payload to route the current agents under control
                  routingActor ! replanningPayload

                  // if we are  stop MATSim until we get our responses
                  if (e.getSimulationTime.toLong < conf.run.endOfRoutingTime.value) {
                    isPaused = true
                    qSim.wait()
                  }
                }
              }
            })

            qSim.addQueueSimulationListeners(new MobsimBeforeCleanupListener {
              override def notifyMobsimBeforeCleanup(
                  e: MobsimBeforeCleanupEvent[_ <: Mobsim]): Unit = {
                val timeOfDay = qSim.getSimTimer.getTimeOfDay
                val safeTime =
                  if (timeOfDay > LocalTime.MAX.toSecondOfDay)
                    s"${(timeOfDay - LocalTime.MAX.toSecondOfDay) / 60D} minutes after end of day"
                  else s"${LocalTime.ofSecondOfDay(timeOfDay.toLong)}"
                log.info(s"ending MATSim simulation at $safeTime")
              }
            })

            qSim.getEventsManager.addHandler(agentsInSimulationHandler)
          }
        })
        MATSimSimulation(controler, qSim, agentsInSimulationHandler, roadNetworkDeltaHandler)
      }

      // synchronous - this thread stops here until we are done running MATSim,
      // but, other threads will be fired off from inside the SOTestbedHandlerListenerModule
      for {
        state <- matsimSimulationState
      } yield {
        for {
          _ <- state.run
          _ = state.controler.run()
        } yield ()
      }

      isDoneWithSimulation = true
      log.info("completed simulation’")

    ///////////////////////////////////////////////// apply routes provided by routing algorithm to MATSim agents
    case MATSimActor.Messages.RouteResponses(time, responses) =>

      for {
        state <- matsimSimulationState
      } {

        // when an agent under control enters the simulation, the simulator begins their trip for them based on selfish routing
        // when we begin a routing re-planning operation, we want to calculate a trip modification start point, somewhere in their future
        //   (we want a "reasonable re-plan travel time buffer" parameter here for all agents under control)
        // for all agents under control at this moment, tracked by state.agentsInSimulationHandler,
        //   check if the re-plan travel time point exists (isn't beyond their destination)
        //   grab that location, their destination (next activity), and their Id[Person]
        // we send all agent data along with a delta of road network state to the routing actor

        // apply changes from RoutePlan
        // we know the agents who are
        //   1. under control
        //   2. have entered the simulation
        //   and we wish to
        // we want to support arbitrary re-planning for agents

        val agentsInSimulation: Map[Id[Person], MobsimAgent] = state.qsim.getAgents.asScala.toMap

        for {
          response    <- responses
          mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))
        } {

          val agentId: Id[Person] = mobsimAgent.getId

          // TODO: update the mobsimAgent with this new path, starting from the spot where this agent crosses the first link in the solution
          //   in other words, the route response path starts at edge e_o. traverse the agent's current (stored) path

          // startLink: agent's current location | first edge in solution
          val startLink: Id[Link] = mobsimAgent.getCurrentLinkId
          // endLink: the end of the Leg
          val endLink: Id[Link] = mobsimAgent.getDestinationLinkId
          // pathPrefix: any bit of path we didn't alter which should still be used
          val pathPrefix: List[Id[Link]] =
            temporaryPathPrefixStore.get(agentId) match {
              case None             => List.empty
              case Some(pathPrefix) => pathPrefix
            }
          // path: all edges between startLink + endLink, if any
          val routingResultPath: List[Id[Link]] = response.path.map { edgeId =>
            Id.createLinkId(edgeId.value)
          }

          // make sure start and end aren't part of the path, and, add the routingResult to the pathPrefix
          val updatedRoute: List[Id[Link]] =
            MATSimOps.coalescePath(startLink, endLink, pathPrefix, routingResultPath)

          // update our local route store
          state.agentsInSimulationHandler.addPathToAgent(agentId, updatedRoute)

          // update the Mobsim
          val updatedRouteAsJava: java.util.List[Id[Link]] = updatedRoute.asJava
          val route: NetworkRoute = WithinDayAgentUtils
            .getModifiableCurrentLeg(mobsimAgent)
            .getRoute
            .asInstanceOf[NetworkRoute]
          route.setLinkIds(startLink, updatedRouteAsJava, endLink)
          WithinDayAgentUtils.resetCaches(mobsimAgent)

          // logging
          if (log.isInfoEnabled) {
            state.agentsInSimulationHandler.getNumberOfPathAssignmentsForAgent(agentId) match {
              case None =>
                throw new IllegalStateException(s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added")
              case Some(numberOfPathAssignments) =>
                log.info(s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $time")
            }
          }
        }

        // reactivate simulation
        isPaused = false
        state.qsim.notify()
      }

    ///////////////////////////////////////////////// check if simulation is done
    case MATSimActor.Messages.IsDoneRequest =>
      // we can't know if the simulation is done until we have played out
      // to a decision point. so
      val timeoutStop: Long = System.currentTimeMillis + messageTimeout.toMillis
      while (isDoneRouting || !isDoneWithSimulation && System.currentTimeMillis < timeoutStop) {
        wait(10000)
      }
      sender() ! MATSimActor.Messages.IsDoneResponse(isDoneWithSimulation)

    case _ => log.error("received unknown message")
  }

  // handle MobsimBeforeSimStepListener to catch the next sim time
  // from inside the event handler,
  //   if advance,
  //     handle events until the next MobsimBeforeSimStepListener
  //     when we encounter the MobsimBeforeSimStepListener, lock and ping the proxy
  //     proxy should have updated network/plan info
}

object MATSimActor {

  sealed trait Messages
  object Messages {
    final case class RouteRequests(
      timeOfDay: Double,
      requests: List[Request],
      networkDeltas: Map[EdgeId, Int]
    )

    final case class RouteResponses(
      timeOfDay: Double,
      responses: List[Response]
    )

    final case object IsDoneRequest
    final case class IsDoneResponse(isDone: Boolean)
  }

  final case class MATSimAgentUnderControlData(
      pathPrefix: List[EdgeId]
  )

  private[MATSimActor] case class ReasonableStartPointFoldAccumulator(
      remainingSlack: TravelTimeSeconds,
      startPoint: Option[EdgeId] = None,
      pathPrefix: List[Id[Link]] = List.empty)

}
