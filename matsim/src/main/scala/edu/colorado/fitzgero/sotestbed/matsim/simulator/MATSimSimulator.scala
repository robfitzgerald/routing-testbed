package edu.colorado.fitzgero.sotestbed.matsim.simulator

import java.io.PrintWriter
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.{Files, Path}
import java.util.concurrent.{Semaphore, TimeUnit}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._

import cats.effect.SyncIO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Flow, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps.SimulatorState
import org.apache.log4j.{Level, Logger}
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{LinkEnterEventHandler, VehicleEntersTrafficEventHandler, VehicleLeavesTrafficEventHandler}
import org.matsim.api.core.v01.events.{LinkEnterEvent, VehicleEntersTrafficEvent, VehicleLeavesTrafficEvent}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.{Leg, Person}
import org.matsim.core.config.{Config, ConfigUtils}
import org.matsim.core.controler.events.{IterationEndsEvent, IterationStartsEvent, ShutdownEvent}
import org.matsim.core.controler.listener.{IterationEndsListener, IterationStartsListener, ShutdownListener}
import org.matsim.core.controler.{AbstractModule, Controler}
import org.matsim.core.events.handler.EventHandler
import org.matsim.core.mobsim.framework.events.{MobsimBeforeSimStepEvent, MobsimInitializedEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeSimStepListener, MobsimInitializedListener}
import org.matsim.core.mobsim.framework.{Mobsim, MobsimAgent, PlayPauseSimulationControl}
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.RouteUtils
import org.matsim.vehicles.Vehicle

/**
  * performs [[SimulatorOps]] on a MATSim simulation which allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  */
trait MATSimSimulator extends SimulatorOps[SyncIO] with LazyLogging { self =>

  override type Simulator              = Unit
  override type SimulatorConfiguration = MATSimRunConfig

  // configuration-based variables
  var endOfRoutingTime: SimTime                                 = _
  var lastIteration: Int                                        = _
  var soFirstIteration: Boolean                                 = _
  var batchWindow: SimTime                                      = _
  var matsimStepSize: SimTime                                   = _
  var matsimSemaphoreTimeoutMs: Long                            = _
  var maxPathAssignments: Int                                   = _
  var reasonableReplanningLeadTime: TravelTimeSeconds           = _
  var minimumRemainingRouteTimeForReplanning: TravelTimeSeconds = _
  var simulationTailTimeout: Duration                           = _

  // simulation state variables
  var matsimState: SimulatorOps.SimulatorState  = SimulatorOps.SimulatorState.Uninitialized
  var matsimOverridingModuleAdded: Boolean      = false
  var observedMATSimIteration: Int              = 0
  var observedHitMidnight: Boolean              = false
  var routingRequestsUpdatedToTimeStep: SimTime = SimTime.Zero
  var soReplanningThisIteration: Boolean        = false

  // simulation state containers and handlers
  var newRouteRequests: Option[RouteRequests]                                                   = None
  var roadNetworkDeltaHandler: RoadNetworkDeltaHandler                                          = _
  var agentsInSimulationNeedingReplanningHandler: AgentsInSimulationNeedingReplanningHandler    = _
  val completePathStore: collection.mutable.Map[Id[Person], Map[DepartureTime, List[Id[Link]]]] = collection.mutable.Map.empty
  val markForPathOverwrite: collection.mutable.Map[Id[Vehicle], Id[Person]]                     = collection.mutable.Map.empty
  var pw: PrintWriter                                                                           = _

  // matsim variables
  var controler: Controler                                   = _
  var qSim: QSim                                             = _
  var playPauseSimulationControl: PlayPauseSimulationControl = _
  var t: Thread                                              = _
  var matsimThreadException: Option[Throwable]               = None
  var access: Option[Semaphore]                              = None

  /**
    * initializes MATSim and registers handlers/listeners which store stateful information via mutable
    * semantics in the scope of [[MATSimSimulator]]
    * @param config configuration relevant to this simulator
    * @return the simulator state object, which is nothing, since all ops are side effects
    */
  override def initializeSimulator(config: SimulatorConfiguration): SyncIO[Simulator] = SyncIO {

    logger.debug("MATSimProxy.initializeSimulator")

    // MATSimProxy config
    self.endOfRoutingTime = config.run.endOfRoutingTime
    self.lastIteration = config.run.lastIteration
    self.soFirstIteration = config.run.soFirstIteration
    self.batchWindow = config.routing.batchWindow
    self.matsimStepSize = config.run.matsimStepSize
    self.matsimSemaphoreTimeoutMs = config.run.matsimSemaphoreTimeoutMs
    self.maxPathAssignments = config.routing.maxPathAssignments
    self.reasonableReplanningLeadTime = config.routing.reasonableReplanningLeadTime
    self.minimumRemainingRouteTimeForReplanning = config.routing.reasonableReplanningLeadTime
    self.simulationTailTimeout = config.run.simulationTailTimeout

    // file system configuration
    val experimentPath: Path = config.io.experimentDirectory
    Files.createDirectories(experimentPath)

    // matsim configuration
    Logger.getLogger("org.matsim").setLevel(Level.toLevel(config.io.matsimLogLevel))
    val matsimConfig: Config = ConfigUtils.loadConfig(config.io.matsimConfigFile.toString)
    matsimConfig.controler.setOutputDirectory(experimentPath.toString)
    matsimConfig.plans.setInputFile(config.io.populationFile.toString)
    matsimConfig.network.setInputFile(config.io.matsimNetworkFile.toString)
    matsimConfig.controler.setLastIteration(config.run.lastIteration)

    // start MATSim and capture object references to simulation in broader MATSimActor scope
    self.controler = new Controler(matsimConfig)

    // needs to happen after the controler checks the experiment directory
    val outputFilePath: String = config.io.experimentLoggingDirectory.resolve("so-stats.txt").toString
    pw = new PrintWriter(outputFilePath)
    pw.write(s"experiment ${config.io.experimentDirectory}\n\n")

    // initialize intermediary data structures holding data between route algorithms + simulation
    self.agentsInSimulationNeedingReplanningHandler = new AgentsInSimulationNeedingReplanningHandler(
      config.pop.agentsUnderControl,
      config.routing.requestUpdateCycle,
      config.routing.maxPathAssignments
    )
    self.roadNetworkDeltaHandler = new RoadNetworkDeltaHandler()

    // track iterations in MATSimProxy
    self.controler.addControlerListener(new IterationStartsListener {
      def notifyIterationStarts(event: IterationStartsEvent): Unit = {
        logger.debug(s"beginning iteration ${event.getIteration}")

        self.observedMATSimIteration = event.getIteration
        self.observedHitMidnight = false

        self.agentsInSimulationNeedingReplanningHandler.clear()
        self.roadNetworkDeltaHandler.clear()

        self.soReplanningThisIteration = if (event.getIteration == 0) {
          // user determines first iteration so routing behavior
          self.soFirstIteration
        } else if (config.run.soRoutingIterationCycle == 0) {
          // prevent divide-by-zero; soRoutingIterationCycle == 0 => no routing
          false
        } else {
          event.getIteration % config.run.soRoutingIterationCycle == 0
        }
      }
    })

    // report end-of-iteration stats, end-of-simulation event
    self.controler.addControlerListener(new IterationEndsListener with ShutdownListener {
      def notifyIterationEnds(event: IterationEndsEvent): Unit = {

        logger.info(s"ending iteration ${event.getIteration}")

        val iterationPrefix: String = s"it-${event.getIteration}"
        val avgPaths: Double        = agentsInSimulationNeedingReplanningHandler.getAvgPathsAssigned
        val avgFailed: Double       = agentsInSimulationNeedingReplanningHandler.getAvgFailedRoutingAttempts
        val sumFailed: Int          = agentsInSimulationNeedingReplanningHandler.getSumFailedRoutingAttempts

        pw.append(s"$iterationPrefix.avg.paths.assigned = $avgPaths\n")
        pw.append(s"$iterationPrefix.avg.failed.routing.attempts = $avgFailed\n")
        pw.append(s"$iterationPrefix.sum.failed.routing.attempts = $sumFailed\n")
      }

      def notifyShutdown(shutdownEvent: ShutdownEvent): Unit = {
        pw.close()
      }
    })

    // inject handlers and listeners for MATSim integration
    self.controler.addOverridingModule(new AbstractModule() { module =>
      @Override def install(): Unit = {

        module
          .addMobsimListenerBinding()
          .toInstance(new MobsimInitializedListener {

            def notifyMobsimInitialized(e: MobsimInitializedEvent[_ <: Mobsim]): Unit = {

              self.qSim = e.getQueueSimulation.asInstanceOf[QSim]
              self.playPauseSimulationControl = new PlayPauseSimulationControl(self.qSim)
              self.playPauseSimulationControl.pause()

              // this is called at the top of every iteration, so we must check to make sure
              // we add exactly one version of each listeners/handler to the qSim
              if (!matsimOverridingModuleAdded) {
                // grab the QSim once it has been initialized so we can add modules to it
                //                self.qSim = e.getQueueSimulation.asInstanceOf[QSim]

                // start the playPause functionality
                //                self.playPauseSimulationControl = new PlayPauseSimulationControl(self.qSim)
                //                self.playPauseSimulationControl.pause()
                // track active agents under control

                self.qSim.getEventsManager.addHandler(agentsInSimulationNeedingReplanningHandler)
                self.qSim.getEventsManager.addHandler(roadNetworkDeltaHandler)

              }
                // handler to force SO agents to use their SO assigned paths at each iteration of MATSim
                self.qSim.getEventsManager.addHandler(new VehicleEntersTrafficEventHandler {
                  override def handleEvent(event: VehicleEntersTrafficEvent): Unit = {

                    // during so-replanning iterations, they are implicitly forced to apply their routes
                    // when not receiving so-replanning routing, the SO agent routes would by default
                    // be assigned by MATSim using the built-in GA policy.
                    if (soReplanningThisIteration) {

                      // noop
                      logger.debug(
                        s"[VehicleEntersTrafficEventHandler] agent ${event.getPersonId} removing stored route in prep for so replanning iteration")
                      // wipe the stored routes, they will be over-written
                      self.completePathStore.remove(event.getPersonId)

                    } else if (agentsInSimulationNeedingReplanningHandler.isUnderControl(event.getPersonId)) {

                      logger.debug(s"[VehicleEntersTrafficEventHandler] triggered for so agent ${event.getPersonId} with stored path")

                      // ITERATION WITHOUT REPLANNING: FLAG TO OVERWRITE THIS AGENT'S ROUTE FOR THIS
                      // DEPARTURE TIME FROM THE COMPLETE PATH STORE

                      // note: delaying the actual overwrite until the agent is actually entering a link,
                      // because i seemed to find agents who are not yet in their Leg that are still
                      // triggering the enter traffic event. so, we simply flag here, and we delay
                      // route modification until we observe this agent's next LinkEnterEvent (below).
                      self.markForPathOverwrite.update(event.getVehicleId, event.getPersonId)
                    }
                  }
                  logger.debug("added person enters vehicle handler")
                })

                self.qSim.getEventsManager.addHandler(new LinkEnterEventHandler {
                  def handleEvent(event: LinkEnterEvent): Unit = {
                    if (!soReplanningThisIteration && self.markForPathOverwrite.isDefinedAt(event.getVehicleId)) {
                      // if we are not replanning, then we want to copy any existing plans for this agent over to MATSim
                      for {
                        personId               <- markForPathOverwrite.get(event.getVehicleId)
                        mobsimAgent            <- qSim.getAgents.asScala.get(personId)
                        leg                    <- MATSimRouteOps.safeGetModifiableLeg(mobsimAgent)
                        completePathsForPerson <- completePathStore.get(personId)
                        pathFromPathStore      <- completePathsForPerson.get(DepartureTime(leg.getDepartureTime.toInt))

                        // checks that there IS a path, and that it's reasonable to assign here
                        if MATSimRouteOps.completePathHasAtLeastTwoLinks(pathFromPathStore)
                      } {
                        // grab stored route and apply it
                        MATSimRouteOps.assignCompleteRouteToLeg(pathFromPathStore, leg)

                        val currentTime: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)
                        logger.debug(
                          s"[LinkEnterEventHandler] $currentTime agent ${personId}: applying stored route with ${pathFromPathStore.length} edges")

                        self.markForPathOverwrite.remove(event.getVehicleId)
                      }
                    }
                  }
                })

                self.qSim.getEventsManager.addHandler(new VehicleLeavesTrafficEventHandler {
                  def handleEvent(event: VehicleLeavesTrafficEvent): Unit = {
                    val agentId: Id[Person] = event.getPersonId
                    if (soReplanningThisIteration && agentsInSimulationNeedingReplanningHandler.isUnderControl(agentId)) {

                      // FINALIZE THIS AGENT'S ROUTE FOR NON-PLANNING ITERATIONS

                      logger.debug(s"[VehicleLeavesTrafficEventHandler] triggered for so agent $agentId")
                      for {
                        mobsimAgent   <- self.qSim.getAgents.asScala.get(agentId)
                        departureTime <- agentsInSimulationNeedingReplanningHandler.getDepartureTimeForAgent(agentId)
                        plan          <- MATSimRouteOps.safeGetModifiablePlan(mobsimAgent)
                        leg           <- MATSimRouteOps.getLegFromPlanByDepartureTime(plan, departureTime)
                        agentExperiencedRoute = MATSimRouteOps.convertToCompleteRoute(leg)
                      } {

                        logger.debug(
                          s"[VehicleLeavesTrafficEventHandler] ${SimTime(self.playPauseSimulationControl.getLocalTime)} agent $agentId: storing completed route with ${agentExperiencedRoute.length} edges")

                        val count: Int = self.agentsInSimulationNeedingReplanningHandler.getReplanningCountForAgent(agentId).getOrElse(0)
                        logger.info(
                          s"[VehicleLeavesTrafficEventHandler] agent $agentId replanned $count times"
                        )

                        // attach this path, keyed by departure time, to the complete list
                        completePathStore.get(agentId) match {
                          case None =>
                            val thisPath: Map[DepartureTime, List[Id[Link]]] =
                              Map(departureTime -> agentExperiencedRoute)
                            completePathStore.update(agentId, thisPath)
                          case Some(alreadyHasPaths) =>
                            completePathStore.update(agentId, alreadyHasPaths.updated(departureTime, agentExperiencedRoute))
                        }
                      }
                    }
                  }
                  logger.debug("added person exits vehicle handler")
                })

                self.qSim.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
                  override def notifyMobsimBeforeSimStep(e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {

                    if (soReplanningThisIteration) {

                      // FIND AGENTS FOR REPLANNING AND STORE REQUESTS FOR THEIR ROUTING

                      val nextSimTime = SimTime(e.getSimulationTime.toInt + 1)

                      if (endOfRoutingTime <= nextSimTime) {
                        // noop
                        logger.debug(
                          s"[MobsimBeforeSimStepListener] time ${e.getSimulationTime} is beyond end of routing time ${endOfRoutingTime.toString} set in config - no routing will occur")
                      } else {

                        logger.debug(s"[MobsimBeforeSimStepListener] finding agents for routing at time ${SimTime(e.getSimulationTime)}")

                        // find the agents who are eligible for re-planning
                        val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
                        val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

                        // convert eligable agents into requests
                        val agentsForReplanning: List[AgentBatchData] =
                          agentsInSimulationNeedingReplanningHandler
                            .getActiveAndEligibleForReplanning(currentSimTime)
                            .foldLeft(List.empty[AgentBatchData]) {
                              (mobsimAgents, agentInSimulation) =>
                                agentsInSimulation.get(agentInSimulation) match {
                                  case None =>
                                    logger.debug(
                                      s"[MobsimBeforeSimStepListener] agent $agentInSimulation that emitted a departure event was not found in QSim - possibly already at destination")
                                    mobsimAgents
                                  case Some(mobsimAgent) =>
                                    Option(WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)) match {
                                      case None =>
                                        logger.error(
                                          s"[MobsimBeforeSimStepListener] agent $agentInSimulation that emitted a departure event does not yet have a trip Leg")
                                        mobsimAgents
                                      case Some(leg) =>
                                        // build Requests for this time step
                                        val agentId           = mobsimAgent.getId
                                        val fullRoute         = MATSimRouteOps.convertToCompleteRoute(leg)
                                        val currentLinkId     = mobsimAgent.getCurrentLinkId
                                        val destinationLinkId = leg.getRoute.getEndLinkId
                                        val destinationEdgeId = EdgeId(destinationLinkId.toString)
                                        MATSimRouteOps.selectRequestOriginLink(fullRoute,
                                          currentLinkId,
                                          destinationLinkId,
                                          self.qSim,
                                          reasonableReplanningLeadTime,
                                          minimumRemainingRouteTimeForReplanning) match {
                                          case None =>
                                            val remainingTT = MATSimRouteOps.estRemainingTravelTimeSeconds(
                                              fullRoute,
                                              currentLinkId,
                                              qSim
                                            )
                                            logger.debug(
                                              f"[MobsimBeforeSimStepListener] didn't find a reasonable edge to attempt replanning for agent $agentId with est. remaining travel time $remainingTT%.2f seconds")
                                            agentsInSimulationNeedingReplanningHandler.incrementNumberFailedRoutingAttempts(agentId)
                                            mobsimAgents
                                          case Some(sourceEdgeId) =>
                                            //                                      agentsInSimulationNeedingReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentSimTime)

                                            val thisRequest: Request =
                                              Request(
                                                agentId.toString,
                                                sourceEdgeId,
                                                destinationEdgeId,
                                                RequestClass.SO(),
                                                TravelMode.Car
                                              )

                                            val routeFromCurrentLink: List[AgentBatchData.EdgeData] =
                                              MATSimRouteOps
                                                .convertToRoutingPath(fullRoute, qSim)
                                                .dropWhile { _.edgeId != EdgeId(currentLinkId.toString) }

                                            val lastReplanningTime: Option[SimTime] =
                                              agentsInSimulationNeedingReplanningHandler.getMostRecentTimePlannedForAgent(agentId)

                                            val thisAgentBatchingData: AgentBatchData =
                                              AgentBatchData(
                                                thisRequest,
                                                currentSimTime,
                                                routeFromCurrentLink,
                                                lastReplanningTime
                                              )

                                            logger.debug(
                                              s"[MobsimBeforeSimStepListener] requesting route for agent $agentId, o=$sourceEdgeId, d=$destinationEdgeId")
                                            thisAgentBatchingData +: mobsimAgents
                                        }
                                    }
                                }
                            }

                        // store any agent routing requests
                        if (agentsForReplanning.isEmpty) {

                          // noop
                          routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)
                          logger.debug(s"[MobsimBeforeSimStepListener] at time ${SimTime(e.getSimulationTime)} has no (new) route requests")

                        } else {

                          // construct the payload for this routing request, replacing whatever route request is currently stored
                          val payload: RouteRequests =
                            RouteRequests(
                              e.getSimulationTime,
                              agentsForReplanning
                            )

                          newRouteRequests = Some { payload }
                          routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)
                          logger.debug(
                            s"[MobsimBeforeSimStepListener] at time ${SimTime(e.getSimulationTime)} storing ${payload.requests.length} requests for batch route module")
                        }
                      }
                    }

                  }
                  logger.debug("added overriding router as before simstep handler")
                })

//                self.matsimOverridingModuleAdded = true
//              }
            }
          })


      }
    })

    self.matsimState = SimulatorOps.SimulatorState.Initialized
    ()
  }

  /**
    * cranks the simulator forward
    * @param simulator the simulator state object
    * @return simulator state after one crank
    */
  override def advance(simulator: Simulator): SyncIO[Simulator] = SyncIO {

    matsimState match {
      case SimulatorState.Initialized =>
        logger.debug("[advance] initializing MATSim")

        t = new Thread(controler)
        t.setUncaughtExceptionHandler(new UncaughtExceptionHandler {
          def uncaughtException(t: Thread, e: Throwable): Unit = {
            self.matsimThreadException = Some { e }
          }
        })
        t.setName("matsim")
        t.start()
        while (self.playPauseSimulationControl == null) {
          Try { Thread.sleep(100) } match {
            case Success(()) => ()
            case Failure(e) =>
              logger.error("[advance] attempting to activate MATSim in child thread, failed:")
              throw e
          }
        }

        matsimState = SimulatorState.Running

        logger.debug("[advance] reached point where simulator can hand off to experiment runner")
        simulator

      case SimulatorState.Running =>
        // crank matsim

        val iterationBeforeCrank: Int = observedMATSimIteration

        // move one sim step forward, or, if "current time" exceeds end of day (from previous iteration),
        // then advance to zero and collect $200.00.

        val currentTime: SimTime      = SimTime(self.playPauseSimulationControl.getLocalTime)
        val advanceToSimTime: SimTime = currentTime + matsimStepSize

        logger.debug(s"[advance] called on sim in Running state: advancing one time step from $currentTime to $advanceToSimTime")

        self.playPauseSimulationControl.doStep(advanceToSimTime.value.toInt)
        self.access = Some { self.playPauseSimulationControl.getAccess }
        // blocks until doStep is completed.
        val startWait: Long = System.currentTimeMillis
        self.access.foreach { _.tryAcquire(self.matsimSemaphoreTimeoutMs, TimeUnit.MILLISECONDS) }
//        access.release()

        val timeAfterAdvance: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)
        val waitDuration: String      = f"${(System.currentTimeMillis - startWait).toDouble / 1000.0}%.2f"

        logger.debug(
          s"[advance] advanced from $currentTime to $timeAfterAdvance (${timeAfterAdvance - currentTime} seconds) observed in $waitDuration seconds runtime")

        val matsimFailure: Boolean                     = self.matsimThreadException.isDefined
        val thisIterationIsFinishedAfterCrank: Boolean = self.playPauseSimulationControl.getLocalTime == Double.MaxValue

        if (matsimFailure) {
          for {
            throwable <- self.matsimThreadException
          } {
            logger.error(s"MATSim failed: ${throwable.getMessage}")
            self.matsimState = SimulatorState.Error(throwable.getMessage)
          }
        } else if (thisIterationIsFinishedAfterCrank) {

          observedHitMidnight = true
          logger.debug("[advance] hit midnight")

          if (iterationBeforeCrank < lastIteration) {
            // we are already on the next iteration - possibly last iteration - though time hasn't restarted.
            // since we aren't on the last iteration, then we should expect time to come around
            // so, let's wait for that to be true.
            var i: Int              = 0
            var stillAlive: Boolean = true
            while (stillAlive && self.playPauseSimulationControl.getLocalTime >= SimTime.EndOfDay.value) {
              Try {
                Thread.sleep(1000)
              } match {
                case Success(()) =>
                  i = i + 1
                  stillAlive = t.isAlive
                  if (!stillAlive) {
                    self.matsimState = SimulatorState.Error("MATSim died transitioning to next iteration")
                  } else {
                    logger.debug(s"[advance] waited $i seconds for time to restart...")
                  }
                case Failure(e) =>
                  logger.error("[advance] waiting for MATSim to start the next iteration, failed:")
                  self.matsimState = SimulatorState.Error(e.getMessage)
              }
            }
          } else {
            logger.debug("[advance] hit end of simulation")
            self.matsimState = SimulatorState.Finishing
          }
        }

        // done cranking
        simulator

      case x =>
        throw new IllegalStateException(s"advance function should never see matsim simulation in state '$x'")
    }
  }

  /**
    * takes the response from the routing algorithm and modifies MATSim agents with the new routes, unless
    * there is a failure, due to stale information or invalid paths
    * @param simulator the simulator state object
    * @param responses a list of agent/route pairs
    * @return the simulator state object
    */
  override def assignReplanningRoutes(simulator: Simulator, responses: List[Response]): SyncIO[Simulator] = SyncIO {

    if (responses.isEmpty) {
      logger.debug(s"[assignRoutes] received 0 route responses - NOOP")

      // if there's a semaphore, release it
      self.access.foreach { _.release() }
      self.access = None

      simulator
    } else {
      logger.debug(s"[assignRoutes] received ${responses.length} route responses")

      val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
      val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

      for {
        response <- responses
        routingResultPath = MATSimRouteOps.convertToMATSimPath(response.path)
        mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))
        if MATSimRouteOps.confirmPathIsValid(routingResultPath, self.qSim) // todo: report invalid paths
      } {

        val leg = WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)

        // it is (somehow) possible for agents which were on a Leg are now on an Activity
        // if they are on an Activity, then, leg will be `null`
        if (leg != null) {

          // extract the mobsim agent data
          val departureTime                         = DepartureTime(leg.getDepartureTime.toInt)
          val agentId: Id[Person]                   = mobsimAgent.getId
          val agentExperiencedRoute: List[Id[Link]] = MATSimRouteOps.convertToCompleteRoute(leg)

          // make sure start and end aren't part of the path, and, add the routingResult to the pathPrefix
          val updatedRoute: List[Id[Link]] =
            MATSimRouteOps.coalescePath(agentExperiencedRoute, routingResultPath)

          logger.debug(s"[assignRoutes] updated route for agent ${agentId.toString} : ${updatedRoute.mkString("->")}")

          // update the mobsim
          val route = RouteUtils.createNetworkRoute(updatedRoute.asJava, qSim.getNetsimNetwork.getNetwork)
          Option(WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)) match {
            case None =>
              logger.error(s"received a new route for agent ${mobsimAgent.getId} but agent does not have a modifiable Leg")
            case Some(modifiableLeg) =>
              modifiableLeg.setRoute(route)
              WithinDayAgentUtils.resetCaches(mobsimAgent)

              // continue to record the experienced agent route associated with this departure time
              completePathStore.get(agentId) match {
                case None =>
                  completePathStore.update(agentId, Map(departureTime -> updatedRoute)) // in-place
                case Some(agentAlreadyRecordedInStore) =>
                  completePathStore.update(agentId, agentAlreadyRecordedInStore.updated(departureTime, updatedRoute)) // in-place
              }

              // doing this now when storing the Request, above, to prevent repeated requests due to PlayPauseSimulationControl
              // taking multiple simulation steps, which leads to duplicate requests in our test runner
              //        agentsInSimulationNeedingReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentSimTime)

              self.agentsInSimulationNeedingReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentSimTime)

              self.agentsInSimulationNeedingReplanningHandler.getReplanningCountForAgent(agentId) match {
                case None =>
                  logger.error(s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added")
                case Some(numberOfPathAssignments) =>
                  logger.debug(s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $currentSimTime")
              }
          }
        }
      }

      // ok! these route requests have been serviced.
      //      newRouteRequests = None

      logger.debug(s"[assignRoutes] modified agents in MATSim based on route responses")

      // if there's a semaphore, release it
      self.access.foreach { _.release() }
      self.access = None

      simulator
    }
  }

  /**
    * finds what [[SimulatorState]] MATSim is in, or, if is has had a failure
    * @param simulator the simulator state object
    * @return either an error message, or, a [[SimulatorOps.SimulatorState]] object
    */
  override def getState(simulator: Simulator): SyncIO[Either[String, SimulatorState]] = SyncIO {

    matsimState match {
      case SimulatorState.Error(msg) => Left(msg)
      case SimulatorState.Finishing =>
        val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

        @tailrec
        def _isDone(): Either[IsDoneFailure, SimulatorState] = {
          if (!t.isAlive) {
            logger.info(s"MATSim Simulation exited normally")
            Right(SimulatorState.Finished)
          } else if (System.currentTimeMillis > timeoutStop) {
            Left(IsDoneFailure.TimeoutFailure(s"surpassed timeout of ${simulationTailTimeout.toMinutes} minutes waiting for simulation to finish"))
          } else {
            Try { Thread.sleep(1000) } match {
              case Success(()) => _isDone()
              case Failure(e) =>
                Left(IsDoneFailure.TimeoutFailure(s"waiting for MATSim in child thread to terminate, failed: ${e.getStackTrace}"))
            }
          }
        }

        playPauseSimulationControl.play()
        _isDone() match {
          case Left(err)            => Left(err.toString)
          case Right(finishedState) => Right(finishedState)
        }
      case SimulatorState.Running if !t.isAlive =>
        matsimState = SimulatorState.Finished
        Left(
          IsDoneFailure.TimeoutFailure(s"MATSim may have had a fatal error").toString
        )

      case other => Right(other)
    }
  }

  /**
    * gets the road network delta
    * @param simulator the simulator state object
    * @return a list of edge id and marginal flow tuples
    */
  override def getUpdatedEdges(simulator: Simulator): SyncIO[List[(EdgeId, Flow)]] = SyncIO {
    val updatedEdges = for {
      (linkId, count) <- roadNetworkDeltaHandler.getDeltas.toList
      if linkId != null // this happened, no idea why
    } yield (EdgeId(linkId.toString), Flow(count))
    roadNetworkDeltaHandler.clear()

//    val numCounts = if (updatedEdges.isEmpty) 0 else updatedEdges.map { _._2.value }.sum
//    logger.info(s"[getUpdatedEdges] has ${updatedEdges.length} entries with aggregate (sum) flow effect of $numCounts")

    updatedEdges
  }

  /**
    * constructs [[Request]] objects for each active agent
    *
    * @return a list of requests
    */
  override def getAgentsNewlyAvailableForReplanning(simulator: Simulator): SyncIO[List[AgentBatchData]] = SyncIO {
    if (matsimState != SimulatorState.Running) {
      List.empty
    } else {

      newRouteRequests match {
        case None =>
          logger.debug(s"[getActiveRequests] returning empty list (no requests)")
          List.empty
        case Some(rr) =>
          logger.debug(s"[getActiveRequests] returning ${rr.requests.size} requests for routing")
          val outgoingRequests = rr.requests
          newRouteRequests = None
          outgoingRequests
      }
    }
  }

  /**
    * returns the SimTime that MATSim is paused at
    * @param simulator the simulator state object
    * @return a [[SimTime]] object representing time in seconds
    */
  override def getCurrentSimTime(simulator: Simulator): SyncIO[SimTime] = SyncIO {
    val currentTime = SimTime(playPauseSimulationControl.getLocalTime)
    logger.debug(s"[getCurrentSimTime] $currentTime")
    currentTime
  }
}
