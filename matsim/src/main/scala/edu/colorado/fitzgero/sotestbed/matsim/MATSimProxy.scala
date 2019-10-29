package edu.colorado.fitzgero.sotestbed.matsim

import java.io.File
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import cats.effect.{IO, Sync, SyncIO}

import com.google.inject.Provider
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment
import edu.colorado.fitzgero.sotestbed.matsim
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.ActivityType
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps.SimulatorState
import org.matsim.api.core.v01.{Id, Scenario}
import org.matsim.api.core.v01.events.{PersonEntersVehicleEvent, PersonLeavesVehicleEvent}
import org.matsim.api.core.v01.events.handler.{PersonEntersVehicleEventHandler, PersonLeavesVehicleEventHandler}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.{Person, Route}
import org.matsim.core.config._
import org.matsim.core.controler.events.IterationStartsEvent
import org.matsim.core.controler.listener.IterationStartsListener
import org.matsim.core.controler.{AbstractModule, Controler}
import org.matsim.core.events.handler.EventHandler
import org.matsim.core.mobsim.framework.events.{MobsimBeforeCleanupEvent, MobsimBeforeSimStepEvent, MobsimInitializedEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeCleanupListener, MobsimBeforeSimStepListener, MobsimInitializedListener}
import org.matsim.core.mobsim.framework.{Mobsim, MobsimAgent, PlayPauseSimulationControl}
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.mobsim.qsim.{QSim, QSimBuilder}
import org.matsim.core.population.routes.NetworkRoute
import org.matsim.core.replanning.selectors.WorstPlanForRemovalSelector
import org.matsim.core.replanning.strategies.DefaultPlanStrategiesModule.{DefaultSelector, DefaultStrategy}
import org.matsim.core.scenario.ScenarioUtils
import org.matsim.core.replanning.strategies._
import org.matsim.withinday.mobsim.MobsimDataProvider

/**
  * performs [[SimulatorOps]] on a MATSim simulation which allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  */
trait MATSimProxy extends SimulatorOps[SyncIO] with LazyLogging { self =>

  override type Simulator              = MATSimSimulation
  override type SimulatorConfiguration = MATSimRunConfig

  var matsimState: SimulatorOps.SimulatorState                                               = SimulatorOps.SimulatorState.Uninitialized
  var matsimStepSize: SimTime                                                                = SimTime(1)
  var endOfRoutingTime: SimTime                                                              = SimTime.EndOfDay
  var lastIteration: Int                                                                     = 0
  var newRouteRequests: Option[MATSimProxy.RouteRequests]                                    = None
  var soReplanningThisIteration: Boolean                                                     = false
  var controler: Controler                                                                   = _
  var qSim: QSim                                                                             = _
  var playPauseSimulationControl: PlayPauseSimulationControl                                 = _
  var roadNetworkDeltaHandler: RoadNetworkDeltaHandler                                       = _
  var agentsInSimulationNeedingReplanningHandler: AgentsInSimulationNeedingReplanningHandler = _
  val completePathStore: collection.mutable.Map[Id[Person], List[Id[Link]]]                  = collection.mutable.Map.empty
  val temporaryPathPrefixStore: collection.mutable.Map[Id[Person], List[Id[Link]]]           = collection.mutable.Map.empty
  var simulationTailTimeout: Duration                                                        = _
  var t: Thread                                                                              = _
  var observedMATSimIteration: Int                                                           = 0
  var observedHitMidnight: Boolean                                                           = false
//  var advanceToSimTime: SimTime = SimTime.Zero

  override def initializeSimulator(config: SimulatorConfiguration): SyncIO[MATSimSimulation] = SyncIO {

    logger.debug("MATSimProxy.initializeSimulator")

    // MATSimProxy config
    endOfRoutingTime = config.run.endOfRoutingTime
    matsimStepSize = config.run.matsimStepSize
    simulationTailTimeout = config.run.simulationTailTimeout
    lastIteration = config.run.lastIteration

    // file system configuration
    val experimentPath: Path =
      config.fs.workingDirectory.resolve(config.fs.experimentSubdirectoryName)
    Files.createDirectories(experimentPath)

    // matsim configuration
//    val matsimConfig: Config = ConfigUtils.createConfig()
    val matsimConfig: Config = ConfigUtils.loadConfig(config.fs.matsimConfigFile.toString)
    matsimConfig.controler.setOutputDirectory(experimentPath.toString)
    matsimConfig.plans.setInputFile(config.fs.populationFile.toString)
    matsimConfig.network.setInputFile(config.fs.matsimNetworkFile.toString)
    matsimConfig.controler.setLastIteration(config.run.lastIteration)

    // start MATSim and capture object references to simulation in broader MATSimActor scope
    controler = new Controler(matsimConfig)

    // initialize intermediary data structures holding data between route algorithms + simulation
    agentsInSimulationNeedingReplanningHandler = new AgentsInSimulationNeedingReplanningHandler(config.pop.agentsUnderControl)
    roadNetworkDeltaHandler = new RoadNetworkDeltaHandler()

    // track iterations in MATSimProxy
    controler.addControlerListener(new IterationStartsListener {
      def notifyIterationStarts(event: IterationStartsEvent): Unit = {
        logger.debug(s"beginning iteration ${event.getIteration}")
        observedMATSimIteration = event.getIteration
        observedHitMidnight = false
//        advanceToSimTime = SimTime.StartOfDay
      }
    })

    // inject handlers and listeners for MATSim integration
    controler.addOverridingModule(new AbstractModule() {
      @Override def install(): Unit = {

        this
          .addMobsimListenerBinding()
          .toInstance(new MobsimInitializedListener {

            def notifyMobsimInitialized(e: MobsimInitializedEvent[_ <: Mobsim]): Unit = {
              self.qSim = e.getQueueSimulation.asInstanceOf[QSim]

              // start the playPause functionality
              playPauseSimulationControl = new PlayPauseSimulationControl(qSim)
              playPauseSimulationControl.pause()

              // track active agents under control
              self.qSim.getEventsManager.addHandler(agentsInSimulationNeedingReplanningHandler)
              self.qSim.getEventsManager.addHandler(roadNetworkDeltaHandler)

              // handler to force SO agents to use their SO assigned paths at each iteration of MATSim
              self.qSim.getEventsManager.addHandler(new PersonEntersVehicleEventHandler {
                override def handleEvent(event: PersonEntersVehicleEvent): Unit = {

                  // during so-replanning iterations, they are implicitly forced to apply their routes
                  // when not receiving so-replanning routing, the SO agent routes would by default
                  // be assigned by MATSim using the built-in GA policy.
                  if (soReplanningThisIteration) {

                    // wipe the stored routes, they will be over-written
                    completePathStore.remove(event.getPersonId)

                  } else if (agentsInSimulationNeedingReplanningHandler.isUnderControl(event.getPersonId)) {

                    // if this person is SO, overwrite their plan and select it, using stored plan
                    for {

//                    person            <- controler.getScenario.getPopulation.getPersons.asScala.get(event.getPersonId)
                      mobsimAgent <- qSim.getAgents.asScala.get(event.getPersonId)
                      pathFromPathStore = completePathStore.getOrElse(event.getPersonId, List.empty[Id[Link]])
//                    requestClass      <- MATSimProxy.getRequestClass(person)
//                    if (requestClass match { case RequestClass.SO(_) => true; case _ => false }) &&
                      if MATSimProxy.pathHasAtLeastTwoLinks(pathFromPathStore)
                    } {

                      val startEdge = pathFromPathStore.head
                      val endEdge   = pathFromPathStore.last
                      val innerPath = pathFromPathStore.tail.drop(1)
                      // grab stored route and apply it
                      WithinDayAgentUtils
                        .getModifiableCurrentLeg(mobsimAgent)
                        .getRoute
                        .asInstanceOf[NetworkRoute]
                        .setLinkIds(startEdge, innerPath.asJava, endEdge)
                    }
                  }
                }
                logger.debug("added person enters vehicle handler")
              })

              self.qSim.getEventsManager.addHandler(new PersonLeavesVehicleEventHandler {
                def handleEvent(event: PersonLeavesVehicleEvent): Unit = {
                  if (soReplanningThisIteration && agentsInSimulationNeedingReplanningHandler.isUnderControl(event.getPersonId)) {
                    for {
//                    person       <- controler.getScenario.getPopulation.getPersons.asScala.get(event.getPersonId)
                      mobsimAgent <- self.qSim.getAgents.asScala.get(event.getPersonId)
//                    requestClass <- MATSimProxy.getRequestClass(person)
//                    if (requestClass match { case RequestClass.SO(_) => true; case _ => false })
                    } {
                      // ok. we finished some SO trip assignment round. store the experienced route.
                      val agentExperiencedRoute: List[Id[Link]] =
                        WithinDayAgentUtils
                          .getModifiableCurrentLeg(mobsimAgent)
                          .getRoute
                          .asInstanceOf[NetworkRoute]
                          .getLinkIds
                          .asScala
                          .toList
                      completePathStore.update(event.getPersonId, agentExperiencedRoute)
                    }
                  }
                }
                logger.debug("added person exits vehicle handler")
              })

              self.qSim.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
                override def notifyMobsimBeforeSimStep(e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {

                  if (e.getSimulationTime.toInt < 0) {
                    // start of simulation. do we replan this iteration?
                    // TODO: may be a cleaner way to get notified about the start of an iteration
                    // MATSim uses -1.0 for the first-observed beforeSimStep call (and Infinity for inactive sims)

                    agentsInSimulationNeedingReplanningHandler.clear()
                    roadNetworkDeltaHandler.clear()

                    soReplanningThisIteration =
                      controler.getIterationNumber % config.run.soRoutingIterationCycle == 0
                  }

                  if (soReplanningThisIteration) {

                    // TODO: confirm that the SO group will use the same plan when no replanning in an iteration
                    // TODO: confirm that the code below can be "skipped"
                    //   maybe solve both problems simulataneously by adding conditional logic to the control flow below
                    //   which only injects new routes when soReplanningThisIteration is true
                    // only perform this re-planning every few iterations allowing for UE equilibrium to take effect between

                    val nextSimTime = SimTime(e.getSimulationTime.toInt + 1)

                    if (config.run.endOfRoutingTime <= nextSimTime) {
                      // noop
                      //  log.info(s"SimTime ${e.getSimulationTime} is beyond end of routing time ${conf.run.endOfRoutingTime} set in config - no routing will occur")
                    } else {

                      // get the changes to the road network observed since last sim step
                      val networkDeltas: Map[EdgeId, Int] = roadNetworkDeltaHandler.getDeltasAsEdgeIds
                      roadNetworkDeltaHandler.clear()

                      // find the agents who are eligible for re-planning
                      val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
                      val agentsEligibleForReplanning: List[MobsimAgent] =
                        agentsInSimulationNeedingReplanningHandler.getActiveAgentIds
                          .foldLeft(List.empty[MobsimAgent]) { (mobsimAgents, agentInSimulation) =>
                            agentsInSimulation.get(agentInSimulation) match {
                              case None =>
                                throw new IllegalStateException(s"agent $agentInSimulation that emitted a departure event was not found in QSim")
                              case Some(mobsimAgent) => mobsimAgent +: mobsimAgents
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
                                agentsInSimulationNeedingReplanningHandler.getPathForAgent(agentId) match {
                                  case None               => List.empty
                                  case Some(previousPath) =>
                                    // catch up with recent history. this can be empty if the current EdgeId isn't on the recorded path
                                    previousPath.dropWhile(_ != currentEdgeId).map {
                                      Id.createLinkId(_)
                                    }
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
                                    Some {
                                      EdgeId(currentEdgeId.toString)
                                    }
                                  case previousPath =>
                                    val reasonableStartPointFoldAccumulator: matsim.MATSimProxy.ReasonableStartPointFoldAccumulator =
                                      previousPath
                                        .map { l =>
                                          val link: Link = self.qSim.getNetsimNetwork
                                            .getNetsimLink(Id.createLinkId(l.toString))
                                            .getLink
                                          (l, Meters.toTravelTime(Meters(link.getLength), MetersPerSecond(link.getFreespeed)))
                                        }
                                        .foldLeft(MATSimProxy.ReasonableStartPointFoldAccumulator(config.routing.reasonableReplanningLeadTime)) {
                                          (acc, tup) =>
                                            acc.startPoint match {
                                              case Some(_) =>
                                                // already found - skip
                                                acc
                                              case None =>
                                                val (linkId, travelTimeSeconds)           = tup
                                                val nextRemainingSlack: TravelTimeSeconds = acc.remainingSlack - travelTimeSeconds
                                                if (nextRemainingSlack <= TravelTimeSeconds.Zero)
                                                  acc.copy(
                                                    remainingSlack = nextRemainingSlack,
                                                    startPoint = Some {
                                                      EdgeId(linkId.toString)
                                                    }
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
                                        agentsInSimulationNeedingReplanningHandler.getNumberOfPathAssignmentsForAgent(agentId) match {
                                          case None =>
                                            throw new IllegalStateException(
                                              s"agent $agentId that emmitted a departure event should be stored in our handler")
                                          case Some(numRoutes) =>
                                            if (numRoutes == 0) {
                                              // ok. agent's trip is short, we still need to find a route, even though
                                              // a route would be shorter than the lower bound of "conf.routing.reasonableReplanningLeadTime"
                                              temporaryPathPrefixStore.update(agentId, List.empty)
                                              Some {
                                                EdgeId(currentEdgeId.toString)
                                              }
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
                                        Some {
                                          startPoint
                                        }
                                    }
                                }

                              // requestOriginOption, if none, means we are cancelling this agent's route request
                              // which is our way of telling the algorithm that the agent's remaining trip is too
                              // short to re-plan
                              //                        TravelMode.fromString(mobsimAgent.getMode) match {
                              //                          // TODO: something parse, handle empty case
                              //                        }
                              requestOriginOption.map { requestOrigin =>
                                Request(agentId.toString, requestOrigin, requestDestination, RequestClass.SO(), TravelMode.Car)
                              }
                          }

                      // construct the payload for this routing request
                      val payload: MATSimProxy.RouteRequests =
                        MATSimProxy.RouteRequests(
                          e.getSimulationTime,
                          agentsForReplanning,
                          networkDeltas
                        )

                      // set aside payload of agents to route
                      newRouteRequests = Some { payload }
                      logger.debug(
                        s"[MobsimBeforeSimStepEvent] at time ${e.getSimulationTime} storing ${payload.requests.length} requests for batch route module")

                    }
                  }
                }
                logger.debug("added overriding router as before simstep handler")
              })
            }
          })

      }
    })

    matsimState = SimulatorOps.SimulatorState.Initialized
    MATSimSimulation()
  }

  override def assignRoutes(simulator: Simulator, responses: List[Response]): SyncIO[Simulator] = SyncIO {

    logger.debug("MATSimProxy.assignRoutes")

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

    if (responses.isEmpty) {
      logger.debug(s"[assignRoutes] received 0 route responses - NOOP")
      simulator
    } else {

      logger.debug(s"[assignRoutes] received ${responses.length} route responses")

      val agentsInSimulation: Map[Id[Person], MobsimAgent] = qSim.getAgents.asScala.toMap

      for {
        response    <- responses
        mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))
      } {

        val agentId: Id[Person] = mobsimAgent.getId

        // agent's current location
        val currentLink: Id[Link] = mobsimAgent.getCurrentLinkId

        // activity location
        val endLink: Id[Link] = mobsimAgent.getDestinationLinkId

        // all links that occurred on this agent's path so far
        val agentExperiencedRoute: List[Id[Link]] =
          WithinDayAgentUtils
            .getModifiableCurrentLeg(mobsimAgent)
            .getRoute
            .asInstanceOf[NetworkRoute]
            .getLinkIds
            .asScala
            .toList
            .takeWhile { _ != currentLink }

        // continue to record the experienced agent route
        completePathStore.update(agentId, completePathStore(agentId) ++ agentExperiencedRoute)

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
        val futureRoute: List[Id[Link]] =
          MATSimOps.coalescePath(currentLink, endLink, pathPrefix, routingResultPath)

        // finally, update the agent's route
        val updatedRoute: List[Id[Link]] = agentExperiencedRoute ++ futureRoute

        // update our local route store
        agentsInSimulationNeedingReplanningHandler.addPathToAgent(agentId, updatedRoute)

        // update the Mobsim
        val updatedRouteAsJava: java.util.List[Id[Link]] = futureRoute.asJava
        val route: NetworkRoute = WithinDayAgentUtils
          .getModifiableCurrentLeg(mobsimAgent)
          .getRoute
          .asInstanceOf[NetworkRoute]
        route.setLinkIds(currentLink, updatedRouteAsJava, endLink)
        //
        //      // TODO: i think internally, MobsimAgent is just keeping an index to their route.
        //      //  if we didn't destroy any experienced route data, and just modified the future,
        //      //  then we might not need to reset their caches here.
        //      WithinDayAgentUtils.resetCaches(mobsimAgent)

        // logging
        //          if (log.isInfoEnabled) {
        //            state.agentsInSimulationHandler.getNumberOfPathAssignmentsForAgent(agentId) match {
        //              case None =>
        //                throw new IllegalStateException(s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added")
        //              case Some(numberOfPathAssignments) =>
        //                log.info(s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $time")
        //            }
        //          }
      }

      // reactivate simulation

      //      isPaused = false
      //      state.qsim.notify()

      logger.debug(s"[assignRoutes] modified agents in MATSim based on route responses")
      //    simulator.playPauseSimulationControl.play()
      simulator
    }
  }

  ///////////////////////////////////////////////// check if simulation is done
  override def getState(simulator: Simulator): SyncIO[Either[String, SimulatorState]] = SyncIO {

    logger.debug("MATSimProxy.getState")

    matsimState match {
      case SimulatorState.Finishing =>
        val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

        @tailrec
        def _isDone(): Either[MATSimSimulation.IsDoneFailure, SimulatorState] = {
          if (!t.isAlive) {
            logger.info(s"MATSim Simulation exited normally")
            Right(SimulatorState.Finished)
          } else if (System.currentTimeMillis > timeoutStop) {
            Left(
              MATSimSimulation.IsDoneFailure.TimeoutFailure(
                s"surpassed timeout of ${simulationTailTimeout.toMinutes} minutes waiting for simulation to finish"))
          } else {
            Try { Thread.sleep(1000) } match {
              case Success(()) => _isDone()
              case Failure(e) =>
                Left(MATSimSimulation.IsDoneFailure.TimeoutFailure(s"waiting for MATSim in child thread to terminate, failed: ${e.getStackTrace}"))
            }
          }
        }

        playPauseSimulationControl.play()
        _isDone() match {
          case Left(err)            => Left(err.toString)
          case Right(finishedState) => Right(finishedState)
        }
      case other => Right(other)
    }
  }

  override def advance(simulator: Simulator): SyncIO[Simulator] = SyncIO {

    // TODO:
    //  is something not updating the PlayPause that the iteration has restarted the time?
    //  is there a lag there?
    //  i think "isFinished" is when local time on PlayPause == Double.PositiveInfinity
    // it gets to the end of iteration 1 normally. what needs to be done to start the next iteration
    // correctly? it just "stops" and after inspection, it seems while we just started iteration 1,
    // the simulation is "finished" according to the PlayPause thingie.

    logger.debug("MATSimProxy.advance")

    if (observedMATSimIteration == 1) {
      logger.debug("Yo")
    }

    matsimState match {
      case SimulatorState.Initialized =>
        logger.debug("[advance] initializing MATSim")

        t = new Thread(controler)
        t.setName("matsim")
        t.start()
        while (self.playPauseSimulationControl == null) {
          Try { Thread.sleep(100) } match {
            case Success(()) => ()
            case Failure(e) =>
              logger.error("attempting to activate MATSim in child thread, failed:")
              throw e
          }
        }

        matsimState = SimulatorState.Running

        logger.debug("[advance] reached point where simulator can hand off to experiment runner")
        simulator

      case SimulatorState.Running =>
        // crank matsim

        logger.debug(s"[advance] moving forward one simstep")

        val iterationBeforeCrank: Int = observedMATSimIteration

        // move one sim step forward, or, if "current time" exceeds end of day (from previous iteration),
        // then advance to zero and collect $200.00.

        val currentTime: SimTime      = SimTime(playPauseSimulationControl.getLocalTime)
        val advanceToSimTime: SimTime = currentTime + matsimStepSize
        playPauseSimulationControl.doStep(advanceToSimTime.value.toInt)

        val thisIterationIsFinishedAfterCrank: Boolean = playPauseSimulationControl.getLocalTime == Double.MaxValue

        if (thisIterationIsFinishedAfterCrank) {

          observedHitMidnight = true
          logger.debug("[advance] hit midnight")

          if (iterationBeforeCrank < lastIteration) {
            // we are already on the next iteration - possibly last iteration - though time hasn't restarted.
            // since we aren't on the last iteration, then we should expect time to come around
            // so, let's wait for that to be true.
            var i = 0
            while (self.playPauseSimulationControl.getLocalTime >= SimTime.EndOfDay.value) {
              Try {
                Thread.sleep(1000)
              } match {
                case Success(()) =>
                  i = i + 1
                  logger.debug(s"[advance] waited $i seconds for time to restart...")
                case Failure(e) =>
                  logger.error("attempting to activate MATSim in child thread, failed:")
                  throw e
              }
            }
          } else {

            logger.debug("[advance] hit end of simulation")
            matsimState = SimulatorState.Finishing
          }
        }

        // done cranking
        simulator

      case x =>
        throw new IllegalStateException(s"advance should never see matsim simulation in state '$x'")
    }

//    if (matsimState == SimulatorState.Initialized) {
//
//      logger.debug("[advance] initializing MATSim")
//
//      t = new Thread(controler)
//      t.setName("matsim")
//      t.start()
//      while (self.playPauseSimulationControl == null) {
//        Try { Thread.sleep(100) } match {
//          case Success(()) => ()
//          case Failure(e) =>
//            logger.error("attempting to activate MATSim in child thread, failed:")
//            throw e
//        }
//      }
//
//      matsimState = SimulatorState.Running
//
//      logger.debug("[advance] reached point where simulator can hand off to experiment runner")
//      simulator
//
//    } else if (matsimState == SimulatorState.Running) {
//
//      // crank matsim
//
//      logger.debug(s"[advance] moving forward one simstep")
//
//      val iterationBeforeCrank: Int = observedMATSimIteration
//
//      // move one sim step forward, or, if "current time" exceeds end of day (from previous iteration),
//      // then advance to zero and collect $200.00.
//
//      val currentTime: SimTime      = SimTime(playPauseSimulationControl.getLocalTime)
//      val advanceToSimTime: SimTime = currentTime + matsimStepSize
//      playPauseSimulationControl.doStep(advanceToSimTime.value.toInt)
//
//      val thisIterationIsFinishedAfterCrank: Boolean = SimTime.EndOfDay.value < playPauseSimulationControl.getLocalTime
//
//      if (thisIterationIsFinishedAfterCrank) {
//
//        observedHitMidnight = true
//        logger.debug("[advance] hit midnight")
//
//        if (iterationBeforeCrank < lastIteration) {
//          // we are already on the next iteration - possibly last iteration - though time hasn't restarted.
//          // since we aren't on the last iteration, then we should expect time to come around
//          // so, let's wait for that to be true.
//          var i = 0
//          while (self.playPauseSimulationControl.getLocalTime >= SimTime.EndOfDay.value) {
//            Try {
//              Thread.sleep(1000)
//            } match {
//              case Success(()) =>
//                i = i + 1
//                logger.debug(s"[advance] waited $i seconds for time to restart...")
//              case Failure(e) =>
//                logger.error("attempting to activate MATSim in child thread, failed:")
//                throw e
//            }
//          }
//        } else {
//
//          logger.debug("[advance] hit end of simulation")
//          matsimState = SimulatorState.Finishing
//
//        }
//      }
//
//      //      } else if (observedHitMidnight && observedMATSimIteration == lastIteration) {
//      //
//      //        // end of day of last iteration, matsim is finishing up
//      //
//      //        matsimState = SimulatorState.Finishing
//      //        simulator
//      //      } else {
//      //
//      //        // noop - maybe between iterations, so thread sleep, and then continue
//      //        Try { Thread.sleep(100) } match {
//      //          case Success(()) =>
//      //            simulator
//      //          case Failure(e) =>
//      //            logger.error("attempting to activate MATSim in child thread, failed:")
//      //            throw e
//      //        }
//      //      }
//      simulator
//    } else {
//      // no op
//      simulator
//    }
  }

  override def getUpdatedEdges(simulator: Simulator): SyncIO[List[(EdgeId, Flow)]] = SyncIO {
    logger.debug("MATSimProxy.getUpdatedEdges")
    for {
      (linkId, count) <- roadNetworkDeltaHandler.getDeltas.toList
    } yield (EdgeId(linkId.toString), Flow(count))
  }

  /**
    * constructs [[Request]] objects for each active agent
    *
    * @return a list of requests
    */
  override def getActiveRequests(simulator: Simulator): SyncIO[List[Request]] = SyncIO {

    logger.debug("MATSimProxy.getActiveRequests")

    val agentLookup  = qSim.getAgents.asScala
    val personLookup = controler.getScenario.getPopulation.getPersons.asScala
    for {

      personId                 <- agentsInSimulationNeedingReplanningHandler.getActiveAgentIds
      mobsimAgent: MobsimAgent <- agentLookup.get(personId).toList
      person: Person           <- personLookup.get(personId).toList
      startLink                  = WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent).getRoute.getStartLinkId
      endLink                    = mobsimAgent.getDestinationLinkId
      requestClass: RequestClass = MATSimProxy.getRequestClass(person).getOrElse(RequestClass.UE)
    } yield {

      Request(
        personId.toString,
        EdgeId(startLink.toString),
        EdgeId(endLink.toString),
        requestClass,
        TravelMode.Car
//        mobsimAgent.getMode
      )
    }
  }

  override def getCurrentSimTime(simulator: Simulator): SyncIO[SimTime] = SyncIO {
    val currentTime = SimTime(playPauseSimulationControl.getLocalTime)
    logger.debug(s"MATSimProxy.getCurrentSimTime $currentTime")
    currentTime
  }
  // handle MobsimBeforeSimStepListener to catch the next sim time
  // from inside the event handler,
  //   if advance,
  //     handle events until the next MobsimBeforeSimStepListener
  //     when we encounter the MobsimBeforeSimStepListener, lock and ping the proxy
  //     proxy should have updated network/plan info
}

object MATSimProxy {

  final case class RouteRequests(
    timeOfDay: Double,
    requests: List[Request],
    networkDeltas: Map[EdgeId, Int]
  )

  final case class RouteResponses(
    timeOfDay: Double,
    responses: List[Response]
  )

  private[MATSimProxy] case class ReasonableStartPointFoldAccumulator(remainingSlack: TravelTimeSeconds,
                                                                      startPoint: Option[EdgeId] = None,
                                                                      pathPrefix: List[Id[Link]] = List.empty)

  val RequestClassAttributeLabel: String = "requestclass"

  def getRequestClass(person: Person): Option[RequestClass] = {
    val requestClassString: String =
      person.getAttributes.getAttribute(RequestClassAttributeLabel).asInstanceOf[String]
    RequestClass(requestClassString)
  }

  def pathHasAtLeastTwoLinks(path: List[Id[Link]]): Boolean = {
    path.nonEmpty && path.tail.drop(1).nonEmpty
  }

  def createScenario(config: Config): Scenario = {
    import org.matsim.core.config.groups.PlanCalcScoreConfigGroup

    import org.matsim.core.scenario.ScenarioUtils
    val sc: Scenario = ScenarioUtils.createScenario(config)

    val pre = new PlanCalcScoreConfigGroup.ActivityParams(ActivityType.Home.toString)
    pre.setTypicalDuration(49) // needs to be geq 49, otherwise when
    // running a simulation one gets
    // "java.lang.RuntimeException: zeroUtilityDuration of type pre-evac must be greater than 0.0. Did you forget to specify the typicalDuration?"
    // the reason is the double precision. see also comment in
    // ActivityUtilityParameters.java (gl)
    pre.setMinimalDuration(49)
    pre.setClosingTime(49)
    pre.setEarliestEndTime(49)
    pre.setLatestStartTime(49)
    pre.setOpeningTime(49)
    val post = new PlanCalcScoreConfigGroup.ActivityParams(ActivityType.Work.toString)
    post.setTypicalDuration(49) // dito
    post.setMinimalDuration(49)
    post.setClosingTime(49)
    post.setEarliestEndTime(49)
    post.setLatestStartTime(49)
    post.setOpeningTime(49)
    config.planCalcScore.addActivityParams(pre)
    config.planCalcScore.addActivityParams(post)
    config.planCalcScore.setLateArrival_utils_hr(0.0)
    config.planCalcScore.setPerforming_utils_hr(0.0)

    // add network, add population happens at config level above.

    sc
  }
}
//////// GRAVEYARD
//        module
//          .bindMobsim()
//          .toProvider(new Provider[Mobsim] {
//            def get(): Mobsim = {
//
//              qSim = new QSimBuilder(matsimConfig)
//                .useDefaults()
//                .build(scenario, controler.getEvents);
//
////              injectedQSimModule = QSimUtils.createDefaultQSim(scenario, controler.getEvents)
//              logger.debug("initialized overriding qSim")
//              self.playPauseSimulationControl = new PlayPauseSimulationControl(qSim)
//              self.playPauseSimulationControl.pause()

//              println()
////            qSim.getEventsManager.addHandler(new EventHandler {
////              override def reset(iteration: Int): Unit = { // OOPS! this is not a handler
////
////                // start in a paused state
//////                logger.debug(s"starting iteration $iteration in paused state")
//////                playPauseSimulationControl.pause()
////
////                // replan on this iteration if it matches the so routing iteration cycle
////                soReplanningThisIteration =
////                  iteration % config.run.soRoutingIterationCycle == 0
////
////              }
////              logger.debug("added reset event handler")
////            })
//
//              // handler to force SO agents to use their SO assigned paths at each iteration of MATSim
//              injectedQSimModule.getEventsManager.addHandler(new PersonEntersVehicleEventHandler {
//                override def handleEvent(event: PersonEntersVehicleEvent): Unit = {
//
//                  // during so-replanning iterations, they are implicitly forced to apply their routes
//                  // when not receiving so-replanning routing, the SO agent routes would by default
//                  // be assigned by MATSim using the built-in GA policy.
//                  if (soReplanningThisIteration) {
//
//                    // wipe the stored routes, they will be over-written
//                    completePathStore.remove(event.getPersonId)
//
//                  } else {
//
//                    // if this person is SO, overwrite their plan and select it, using stored plan
//                    for {
//                      pathFromPathStore <- completePathStore.get(event.getPersonId)
//                      person            <- controler.getScenario.getPopulation.getPersons.asScala.get(event.getPersonId)
//                      mobsimAgent       <- injectedQSimModule.getAgents.asScala.get(event.getPersonId)
//                      requestClass      <- MATSimProxy.getRequestClass(person)
//                      if (requestClass match { case RequestClass.SO(_) => true; case _ => false }) &&
//                        pathFromPathStore.tail.drop(1).nonEmpty
//                    } {
//
//                      val startEdge = pathFromPathStore.head
//                      val endEdge   = pathFromPathStore.last
//                      val innerPath = pathFromPathStore.tail.drop(1)
//                      // grab stored route and apply it
//                      WithinDayAgentUtils
//                        .getModifiableCurrentLeg(mobsimAgent)
//                        .getRoute
//                        .asInstanceOf[NetworkRoute]
//                        .setLinkIds(startEdge, innerPath.asJava, endEdge)
//                    }
//                  }
//                }
//                logger.debug("added person enters vehicle handler")
//              })
//
//              injectedQSimModule.getEventsManager.addHandler(new PersonLeavesVehicleEventHandler {
//                def handleEvent(event: PersonLeavesVehicleEvent): Unit = {
//                  if (soReplanningThisIteration) {
//                    for {
//                      person       <- controler.getScenario.getPopulation.getPersons.asScala.get(event.getPersonId)
//                      mobsimAgent  <- injectedQSimModule.getAgents.asScala.get(event.getPersonId)
//                      requestClass <- MATSimProxy.getRequestClass(person)
//                      if (requestClass match { case RequestClass.SO(_) => true; case _ => false })
//                    } {
//                      // ok. we finished some SO trip assignment round. store the experienced route.
//                      val agentExperiencedRoute: List[Id[Link]] =
//                        WithinDayAgentUtils
//                          .getModifiableCurrentLeg(mobsimAgent)
//                          .getRoute
//                          .asInstanceOf[NetworkRoute]
//                          .getLinkIds
//                          .asScala
//                          .toList
//                      completePathStore.update(event.getPersonId, agentExperiencedRoute)
//                    }
//                  }
//                }
//                logger.debug("added person exits vehicle handler")
//              })
//
//              injectedQSimModule.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
//                override def notifyMobsimBeforeSimStep(e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {
//
//                  // start this thing paused
////                if (e.getSimulationTime == Double.PositiveInfinity && controler.getIterationNumber == 0) {
////                  logger.debug("[MobsimBeforeSimStepEvent] pausing simulation")
////                  playPauseSimulationControl.play()
////                  playPauseSimulationControl
////                }
//
//                  if (e.getSimulationTime.toInt < 0) {
//                    // start of simulation. do we replan this iteration?
//                    // MATSim uses -1.0 for the first-observed beforeSimStep call (and Infinity for inactive sims)
//
//                    soReplanningThisIteration =
//                      controler.getIterationNumber % config.run.soRoutingIterationCycle == 0
//                  }
//
//                  if (soReplanningThisIteration) {
//
//                    // TODO: confirm that the SO group will use the same plan when no replanning in an iteration
//                    // TODO: confirm that the code below can be "skipped"
//                    //   maybe solve both problems simulataneously by adding conditional logic to the control flow below
//                    //   which only injects new routes when soReplanningThisIteration is true
//                    // only perform this re-planning every few iterations allowing for UE equilibrium to take effect between
//
//                    val nextSimTime = SimTime(e.getSimulationTime.toInt + 1)
//
//                    if (config.run.endOfRoutingTime <= nextSimTime) {
//                      // noop
//                      //  log.info(s"SimTime ${e.getSimulationTime} is beyond end of routing time ${conf.run.endOfRoutingTime} set in config - no routing will occur")
//                    } else {
//
//                      // get the changes to the road network observed since last sim step
//                      val networkDeltas: Map[EdgeId, Int] = roadNetworkDeltaHandler.getDeltasAsEdgeIds
//                      roadNetworkDeltaHandler.clear()
//
//                      // find the agents who are eligible for re-planning
//                      val agentsInSimulation: Map[Id[Person], MobsimAgent] =
//                        e.getQueueSimulation.asInstanceOf[QSim].getAgents.asScala.toMap
//                      val agentsEligibleForReplanning: List[MobsimAgent] =
//                        agentsInSimulationHandler.getActiveAgentIds
//                          .foldLeft(List.empty[MobsimAgent]) { (mobsimAgents, agentInSimulation) =>
//                            agentsInSimulation.get(agentInSimulation) match {
//                              case None =>
//                                throw new IllegalStateException(s"agent $agentInSimulation that emitted a departure event was not found in QSim")
//                              case Some(mobsimAgent) => mobsimAgent +: mobsimAgents
//                            }
//                          }
//
//                      // convert the active agents eligible for re-planning into a List of Requests
//                      val agentsForReplanning: List[Request] =
//                        agentsEligibleForReplanning
//                          .flatMap {
//                            mobsimAgent: MobsimAgent =>
//                              // TODO: turn into a route request. we want to
//                              //   1. see if agentsInSimulationHandler has a path, and, if so,
//                              //     try and estimate a good point to modify the agent's remaining path
//                              //     or just pass the current point they are at if they are new to the routing algorithm
//                              //   2. pass this origin, and their known destination (mobsimAgent.getDestinationLinkId) to the route planner
//
//                              // MISSING: a way to estimate travel time over the road network links
//                              val agentId: Id[Person]     = mobsimAgent.getId
//                              val currentEdgeId: Id[Link] = mobsimAgent.getCurrentLinkId
//
//                              // we want the remaining previously-assigned route starting from the agent's current Id[Link]
//                              val previousPathFromCurrentOrigin: List[Id[Link]] =
//                                agentsInSimulationHandler.getPathForAgent(agentId) match {
//                                  case None               => List.empty
//                                  case Some(previousPath) =>
//                                    // catch up with recent history. this can be empty if the current EdgeId isn't on the recorded path
//                                    previousPath.dropWhile(_ != currentEdgeId).map {
//                                      Id.createLinkId(_)
//                                    }
//                                }
//
//                              // the easy part...
//                              val requestDestination: EdgeId =
//                                EdgeId(mobsimAgent.getDestinationLinkId.toString)
//
//                              // pick a reasonable starting point.
//                              // we want to find a point that will take at least "conf.routing.reasonableReplanningLeadTime" to reach
//                              // which we compute based on the free flow travel time for the remaining links.
//                              val requestOriginOption: Option[EdgeId] =
//                                previousPathFromCurrentOrigin match {
//                                  case Nil =>
//                                    // no previously assigned route, so, start from currentEdgeId
//                                    Some {
//                                      EdgeId(currentEdgeId.toString)
//                                    }
//                                  case previousPath =>
//                                    val reasonableStartPointFoldAccumulator: matsim.MATSimProxy.ReasonableStartPointFoldAccumulator =
//                                      previousPath
//                                        .map { l =>
//                                          val link: Link = injectedQSimModule.getNetsimNetwork
//                                            .getNetsimLink(Id.createLinkId(l.toString))
//                                            .getLink
//                                          (l, Meters.toTravelTime(Meters(link.getLength), MetersPerSecond(link.getFreespeed)))
//                                        }
//                                        .foldLeft(MATSimProxy.ReasonableStartPointFoldAccumulator(config.routing.reasonableReplanningLeadTime)) {
//                                          (acc, tup) =>
//                                            acc.startPoint match {
//                                              case Some(_) =>
//                                                // already found - skip
//                                                acc
//                                              case None =>
//                                                val (linkId, travelTimeSeconds)           = tup
//                                                val nextRemainingSlack: TravelTimeSeconds = acc.remainingSlack - travelTimeSeconds
//                                                if (nextRemainingSlack <= TravelTimeSeconds.Zero)
//                                                  acc.copy(
//                                                    remainingSlack = nextRemainingSlack,
//                                                    startPoint = Some {
//                                                      EdgeId(linkId.toString)
//                                                    }
//                                                  )
//                                                else
//                                                  acc.copy(
//                                                    remainingSlack = nextRemainingSlack,
//                                                    pathPrefix = acc.pathPrefix :+ linkId
//                                                  )
//                                            }
//                                        }
//
//                                    // if there isn't a reasonable future replanning point, then we shouldn't disturb this agent
//
//                                    reasonableStartPointFoldAccumulator.startPoint match {
//                                      case None =>
//                                        agentsInSimulationHandler.getNumberOfPathAssignmentsForAgent(agentId) match {
//                                          case None =>
//                                            throw new IllegalStateException(
//                                              s"agent $agentId that emmitted a departure event should be stored in our handler")
//                                          case Some(numRoutes) =>
//                                            if (numRoutes == 0) {
//                                              // ok. agent's trip is short, we still need to find a route, even though
//                                              // a route would be shorter than the lower bound of "conf.routing.reasonableReplanningLeadTime"
//                                              temporaryPathPrefixStore.update(agentId, List.empty)
//                                              Some {
//                                                EdgeId(currentEdgeId.toString)
//                                              }
//                                            } else {
//                                              // ok, this agent's remaining trip is too short as it is below the lower bound
//                                              // provided by "conf.routing.reasonableReplanningLeadTime",
//                                              // so we won't send a re-routing request here
//                                              None
//                                            }
//                                        }
//                                      case Some(startPoint) =>
//                                        temporaryPathPrefixStore
//                                          .update(agentId, reasonableStartPointFoldAccumulator.pathPrefix)
//                                        Some {
//                                          startPoint
//                                        }
//                                    }
//                                }
//
//                              // requestOriginOption, if none, means we are cancelling this agent's route request
//                              // which is our way of telling the algorithm that the agent's remaining trip is too
//                              // short to re-plan
//                              //                        TravelMode.fromString(mobsimAgent.getMode) match {
//                              //                          // TODO: something parse, handle empty case
//                              //                        }
//                              requestOriginOption.map { requestOrigin =>
//                                Request(agentId.toString, requestOrigin, requestDestination, RequestClass.SO(), TravelMode.Car)
//                              }
//                          }
//
//                      // construct the payload for this routing request
//                      val payload: MATSimProxy.RouteRequests =
//                        MATSimProxy.RouteRequests(
//                          e.getSimulationTime,
//                          agentsForReplanning,
//                          networkDeltas
//                        )
//
//                      // set aside payload of agents to route
//                      newRouteRequests = Some { payload }
//                      logger.debug(
//                        s"[MobsimBeforeSimStepEvent] at time ${e.getSimulationTime} storing ${payload.requests.length} requests for batch route module")
//
//                    }
//                  }
//                }
//                logger.debug("added overriding router as before simstep handler")
//              })
//
//              logger.debug("adding agents in simulation event handler")
//              self.qSim.getEventsManager.addHandler(agentsInSimulationNeedingReplanningHandler)
//
//              qSim
//            }
//          })
//      }
//    })

// initialize the shared state of the simulation
//    MATSimSimulation()
//  }
