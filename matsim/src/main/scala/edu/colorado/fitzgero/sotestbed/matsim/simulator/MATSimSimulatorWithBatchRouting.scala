package edu.colorado.fitzgero.sotestbed.matsim.simulator

import java.io.PrintWriter
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator.SimulatorState
import org.apache.log4j.{Level, Logger}
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{
  LinkEnterEventHandler,
  VehicleEntersTrafficEventHandler,
  VehicleLeavesTrafficEventHandler
}
import org.matsim.api.core.v01.events.{LinkEnterEvent, VehicleEntersTrafficEvent, VehicleLeavesTrafficEvent}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.{Activity, Person}
import org.matsim.core.config.{Config, ConfigUtils}
import org.matsim.core.controler.events.{IterationEndsEvent, IterationStartsEvent, ShutdownEvent}
import org.matsim.core.controler.listener.{IterationEndsListener, IterationStartsListener, ShutdownListener}
import org.matsim.core.controler.{AbstractModule, Controler}
import org.matsim.core.mobsim.framework.events.{MobsimBeforeSimStepEvent, MobsimInitializedEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeSimStepListener, MobsimInitializedListener}
import org.matsim.core.mobsim.framework.{Mobsim, MobsimAgent, PlayPauseControlSecondStepper}
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.RouteUtils
import org.matsim.core.trafficmonitoring.TravelTimeCalculator
import org.matsim.vehicles.Vehicle
import org.matsim.core.trafficmonitoring.TravelTimeCalculatorModule
import org.matsim.core.router.util.TravelTime

/**
  * performs [[HandCrankedSimulator]] on a MATSim simulation which allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  */
trait MATSimSimulatorWithBatchRouting extends HandCrankedSimulator[IO] with LazyLogging { self =>

  override type SimulatorConfiguration = MATSimRunConfig

  // configuration-based variables
  var endOfRoutingTime: SimTime                                 = _
  var lastIteration: Int                                        = _
  var soFirstIteration: Boolean                                 = _
  var soRoutingIterationCycle: Int                              = _
  var useExternalRoutingEngineForSelfishAgents: Boolean         = _
  var batchWindow: SimTime                                      = _
  var matsimStepSize: SimTime                                   = _
  var matsimSemaphoreTimeoutMs: Long                            = _
  var maxPathAssignments: Int                                   = _
  var minimumReplanningLeadTime: TravelTimeSeconds              = _
  var minimumRemainingRouteTimeForReplanning: TravelTimeSeconds = _
  var simulationTailTimeout: Duration                           = _

  // simulation state variables
  var matsimState: HandCrankedSimulator.SimulatorState = HandCrankedSimulator.SimulatorState.Uninitialized
  var matsimOverridingModuleAdded: Boolean             = false
  var observedMATSimIteration: Int                     = 0
  var observedHitMidnight: Boolean                     = false
  var routingRequestsUpdatedToTimeStep: SimTime        = SimTime.Zero
  var soReplanningThisIteration: Boolean               = false
  var reachedDestination: Int                          = 0
  var selfishAgentRoutesAssigned: Int                  = 0

  // simulation state containers and handlers
  var roadNetworkDeltaHandler: RoadNetworkDeltaHandler   = _
  var soAgentReplanningHandler: SOAgentReplanningHandler = _

  val completePathStore: collection.mutable.Map[Id[Person], Map[DepartureTime, List[Id[Link]]]] =
    collection.mutable.Map.empty
  val departureTimeStore: collection.mutable.Map[Id[Person], DepartureTime]   = collection.mutable.Map.empty
  val markForSOPathOverwrite: collection.mutable.Map[Id[Vehicle], Id[Person]] = collection.mutable.Map.empty
  val markForUEPathOverwrite: collection.mutable.Map[Id[Vehicle], Id[Person]] = collection.mutable.Map.empty
  var newSORouteRequests: Option[RouteRequests]                               = None
  val newUERouteRequests: collection.mutable.ListBuffer[AgentBatchData]       = collection.mutable.ListBuffer.empty
  val newSOAgentBatchData: collection.mutable.ListBuffer[AgentBatchData]      = collection.mutable.ListBuffer.empty
  val ueAgentAssignedDijkstraRoute: collection.mutable.Set[Id[Person]]        = collection.mutable.Set.empty

  var agentLeftSimulationRequests: collection.mutable.ListBuffer[SOAgentArrivalData] =
    collection.mutable.ListBuffer.empty
  var runStatsPrintWriter: PrintWriter        = _
  var agentExperiencePrintWriter: PrintWriter = _
  var agentPathPrintWriter: PrintWriter       = _

  // matsim variables
  var controler: Controler                                      = _
  var qSim: QSim                                                = _
  var playPauseSimulationControl: PlayPauseControlSecondStepper = _
  var t: Thread                                                 = _
  var matsimThreadException: Option[Throwable]                  = None
  var travelTimeCalculatorModule: TravelTimeCalculatorModule    = _

  /**
    * initializes MATSim and registers handlers/listeners which store stateful information via mutable
    * semantics in the scope of [[MATSimSimulatorWithBatchRouting]]
    *
    * @param config configuration relevant to this simulator
    * @return the simulator state object, which is nothing, since all ops are side effects
    */
  override def initializeSimulator(config: SimulatorConfiguration): IO[Unit] = IO {

    logger.debug("MATSimProxy.initializeSimulator")

    // MATSimProxy config
    self.endOfRoutingTime = config.run.endOfRoutingTime
    self.batchWindow = config.routing.batchWindow
    self.matsimStepSize = config.run.matsimStepSize
    self.matsimSemaphoreTimeoutMs = config.run.matsimSemaphoreTimeoutMs
    self.maxPathAssignments = config.routing.maxPathAssignments
    self.minimumReplanningLeadTime = config.routing.minimumReplanningLeadTime
    self.minimumRemainingRouteTimeForReplanning = config.routing.minimumReplanningLeadTime
    self.simulationTailTimeout = config.run.simulationTailTimeout
//    self.onlySelfishAgents = config.algorithm match {
//      case _: MATSimConfig.Algorithm.Selfish => true
//      case _                                 => false
//    }

    // MATSimProxy Selfish Routing Mode
    config.routing.selfish match {
      case MATSimConfig.Routing.Selfish.Matsim(lastIter, soRoutingIterCycle, soFirstIter) =>
        // allow MATSim's genetic algorithm to attempt to find a user equilibrium for selfish agents
        self.lastIteration = lastIter
        self.soFirstIteration = soFirstIter
        self.soRoutingIterationCycle = soRoutingIterCycle
        self.useExternalRoutingEngineForSelfishAgents = false
      case _: MATSimConfig.Routing.Selfish.Dijkstra =>
        // override MATSim's genetic algorithm with an external selfish routing algorithm
        self.lastIteration = 0
        self.soFirstIteration = true
        self.soRoutingIterationCycle = 0
        self.useExternalRoutingEngineForSelfishAgents = true
    }

    // file system configuration
    val experimentPath: Path = config.experimentDirectory
    Files.createDirectories(experimentPath)
    logger.info(s"experiment path: $experimentPath")

    // matsim configuration
    Logger.getLogger("org.matsim").setLevel(Level.toLevel(config.io.matsimLogLevel))
    val matsimConfig: Config = ConfigUtils.loadConfig(config.io.matsimConfigFile.toString)
    matsimConfig.controler.setOutputDirectory(experimentPath.toString)
    matsimConfig.plans.setInputFile(config.io.populationFile.toString)
    matsimConfig.network.setInputFile(config.io.matsimNetworkFile.toString)
    matsimConfig.controler.setLastIteration(config.routing.selfish.lastIteration)

    // start MATSim and capture object references to simulation in broader MATSimActor scope
    self.controler = new Controler(matsimConfig)

    //
    // self.travelTimeCalculatorModule = new TravelTimeCalculatorModule()
    // self.controler.addOverridingModule(travelTimeCalculatorModule)

    // needs to happen after the controler checks the experiment directory (20200125-is this still true?)
    val statsFilePath: String =
      config.experimentLoggingDirectory.resolve(s"stats-${config.algorithm.name}.txt").toString
    runStatsPrintWriter = new PrintWriter(statsFilePath)
    runStatsPrintWriter.write(s"experiment $experimentPath\n\n")

    val agentExperienceFilePath: String = config.experimentLoggingDirectory.resolve(s"agentExperience.csv").toString
    agentExperiencePrintWriter = new PrintWriter(agentExperienceFilePath)
    agentExperiencePrintWriter.write("agentId,requestClass,departureTime,travelTime,distance,replannings\n")

    val agentPathFilePath: String = config.experimentLoggingDirectory.resolve(s"agentPath.csv").toString
    agentPathPrintWriter = new PrintWriter(agentPathFilePath)
    agentPathPrintWriter.write("agentId,WKT\n")

    // initialize intermediary data structures holding data between route algorithms + simulation
    self.soAgentReplanningHandler = new SOAgentReplanningHandler(
      config.agentsUnderControl,
      config.routing.maxPathAssignments,
      config.routing.minimumReplanningWaitTime
    )
    self.roadNetworkDeltaHandler = new RoadNetworkDeltaHandler(config.routing.minNetworkUpdateThreshold)

    // track iterations in MATSimProxy
    self.controler.addControlerListener(new IterationStartsListener {
      def notifyIterationStarts(event: IterationStartsEvent): Unit = {
        logger.debug(s"beginning iteration ${event.getIteration}")

        self.observedMATSimIteration = event.getIteration
        self.observedHitMidnight = false
        self.reachedDestination = 0

        self.selfishAgentRoutesAssigned = 0

        if (self.markForSOPathOverwrite.nonEmpty) {
          logger.warn(
            s"found ${markForSOPathOverwrite.size} agents marked for SO path overwrite which were never transformed into requests"
          )
        }
        if (self.markForUEPathOverwrite.nonEmpty) {
          logger.warn(
            s"found ${markForUEPathOverwrite.size} agents marked for UE path overwrite which were never transformed into requests"
          )
        }

        self.markForSOPathOverwrite.clear()
        self.markForUEPathOverwrite.clear()
        self.ueAgentAssignedDijkstraRoute.clear()
        self.departureTimeStore.clear()
        self.soAgentReplanningHandler.clear()
        self.roadNetworkDeltaHandler.clear()

        self.soReplanningThisIteration = if (event.getIteration == 0) {
          // user determines first iteration so routing behavior
          self.soFirstIteration
        } else if (self.soRoutingIterationCycle == 0) {
          // prevent divide-by-zero; soRoutingIterationCycle == 0 => no routing
          false
        } else {
          event.getIteration % self.soRoutingIterationCycle == 0
        }
      }
    })

    // report end-of-iteration stats, end-of-simulation event
    self.controler.addControlerListener(new IterationEndsListener with ShutdownListener {
      def notifyIterationEnds(event: IterationEndsEvent): Unit = {

        logger.info(s"ending iteration ${event.getIteration}")

        val iterationPrefix: String = s"it-${event.getIteration}"
        val avgPaths: Double        = soAgentReplanningHandler.getAvgPathsAssigned
        val avgFailed: Double       = soAgentReplanningHandler.getAvgFailedRoutingAttempts
        val sumFailed: Int          = soAgentReplanningHandler.getSumFailedRoutingAttempts

        runStatsPrintWriter.append(s"$iterationPrefix.sum.selfish.assignments = $selfishAgentRoutesAssigned\n")
        runStatsPrintWriter.append(s"$iterationPrefix.avg.paths.assigned = $avgPaths\n")
        runStatsPrintWriter.append(s"$iterationPrefix.avg.failed.routing.attempts = $avgFailed\n")
        runStatsPrintWriter.append(s"$iterationPrefix.sum.failed.routing.attempts = $sumFailed\n")
        runStatsPrintWriter.append(s"$iterationPrefix.sum.reached_destination = ${self.reachedDestination}\n")
      }

      def notifyShutdown(shutdownEvent: ShutdownEvent): Unit = {
        runStatsPrintWriter.close()
        agentExperiencePrintWriter.close()
        agentPathPrintWriter.close()
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
              self.playPauseSimulationControl = new PlayPauseControlSecondStepper(self.qSim)
              self.playPauseSimulationControl.pause()

              // this is called at the top of every iteration, so we must check to make sure
              // we add exactly one version of each listeners/handler to the qSim
              if (!matsimOverridingModuleAdded) {

                self.qSim.getEventsManager.addHandler(soAgentReplanningHandler)
                self.qSim.getEventsManager.addHandler(roadNetworkDeltaHandler)

              }
              // handler to force SO agents to use their SO assigned paths at each iteration of MATSim
              self.qSim.getEventsManager.addHandler(new VehicleEntersTrafficEventHandler {
                override def handleEvent(event: VehicleEntersTrafficEvent): Unit = {

                  Try {

                    val personId      = event.getPersonId
                    val vehicleId     = event.getVehicleId
                    val departureTime = DepartureTime(event.getTime.toInt)
                    val simTime       = SimTime(event.getTime.toInt)

                    self.departureTimeStore.update(personId, departureTime)

                    if (self.useExternalRoutingEngineForSelfishAgents &&
                        !soAgentReplanningHandler.isUnderControl(personId)) {
                      // this is a UE agent who needs selfish routing. we will
                      // mark them to have dijkstra routing when they enter their
                      // first path edge

                      for {
                        mobsimAgent <- self.qSim.getAgents.asScala.get(personId)
                        homeMorningActivity <- WithinDayAgentUtils
                          .getModifiablePlan(mobsimAgent)
                          .getPlanElements
                          .asScala
                          .toList
                          .headOption
                          .map {
                            _.asInstanceOf[Activity]
                          }
                      } {
                        val planDepartureTime = SimTime(homeMorningActivity.getEndTime.seconds.toInt)
                        val eventTimes        = f"$planDepartureTime $simTime"

                        val timeDiff = simTime - planDepartureTime
                        homeMorningActivity
                      }

                      self.markForUEPathOverwrite.update(vehicleId, personId)
                      self.ueAgentAssignedDijkstraRoute -= personId
                      logger.debug(s"agent $personId marked for routing")

                    } else if (soReplanningThisIteration) {
                      // during so-replanning iterations, they are implicitly forced to apply their routes
                      // when not receiving so-replanning routing, the SO agent routes would by default
                      // be assigned by MATSim using the built-in GA policy.
                      // noop

                      // wipe the stored routes, they will be over-written
                      self.completePathStore.remove(personId)

                      // let the routing algorithm know this agent has entered the system
                      val entersMessage = AgentBatchData.EnterSimulation(personId.toString, simTime)
                      self.newSOAgentBatchData.prepend(entersMessage)

                      logger
                        .debug(s"agent $personId removing stored route in prep for so replanning iteration")

                    } else {

                      // ITERATION WITHOUT REPLANNING: FLAG TO OVERWRITE THIS AGENT'S ROUTE FOR THIS
                      // DEPARTURE TIME FROM THE COMPLETE PATH STORE

                      // note: delaying the actual overwrite until the agent is actually entering a link,
                      // because i seemed to find agents who are not yet in their Leg that are still
                      // triggering the enter traffic event. so, we simply flag here, and we delay
                      // route modification until we observe this agent's next LinkEnterEvent (below).
                      self.markForSOPathOverwrite.update(vehicleId, personId)

                      logger.debug(s"triggered for so agent $personId with stored path")
                    }

                  } match {
                    case util.Failure(e) =>
                      logger.error("VehicleEntersTrafficEvent failure")
                      throw e
                    case util.Success(_) => ()
                  }
                }
                logger.debug("added person enters vehicle handler")
              })

              self.qSim.getEventsManager.addHandler(new LinkEnterEventHandler {
                def handleEvent(event: LinkEnterEvent): Unit = {

                  Try {

                    if (self.markForUEPathOverwrite.isDefinedAt(event.getVehicleId)) {
                      // request a Dijkstra's shortest path for this agent

                      for {
                        personId    <- self.markForUEPathOverwrite.get(event.getVehicleId)
                        mobsimAgent <- self.qSim.getAgents.asScala.get(personId)
                        currentLinkId = mobsimAgent.getCurrentLinkId
                        leg <- MATSimRouteOps.safeGetModifiableLeg(mobsimAgent)
                        fullRoute        = MATSimRouteOps.convertToCompleteRoute(leg)
                        experiencedRoute = fullRoute.takeWhile(_ != currentLinkId)
                        remaingingRoute  = fullRoute.dropWhile(_ != currentLinkId)
                        remainingDistance <- MATSimRouteOps.distanceOfPath(remaingingRoute, qSim)
                        originLinkId <- fullRoute
                          .dropWhile(_ != currentLinkId)
                          .headOption // confirms MATSim currentLinkId is correct
                        endLinkId            = leg.getRoute.getEndLinkId
                        requestDepartureTime = SimTime(event.getTime.toInt)
                      } {

                        // construct an AgentBatchData payload to request selfish routing
                        // starts at the link after the current link in the current path
                        // if there is no link after the current link, then originLinkId fails fast

                        MATSimRouteOps.selectRequestOriginLink(
                          fullRoute,
                          originLinkId,
                          endLinkId,
                          self.qSim,
                          self.minimumReplanningLeadTime,
                          self.minimumRemainingRouteTimeForReplanning
                        ) match {
                          case None =>
                            val remainingTT: Double =
                              MATSimRouteOps.estRemainingTravelTimeSeconds(fullRoute, currentLinkId, qSim)

                            logger.debug(
                              f"didn't find a reasonable edge to attempt replanning for agent $personId with est. remaining travel time $remainingTT%.2f seconds"
                            )

                          case Some(reasonableStartEdgeId) =>
                            val currentSimTime: SimTime = SimTime(event.getTime.toInt)
                            val thisRequest: Request =
                              Request(
                                agent = personId.toString,
                                location = reasonableStartEdgeId,
                                destination = EdgeId(endLinkId.toString),
                                requestClass = RequestClass.UE,
                                travelMode = TravelMode.Car,
                                departureTime = requestDepartureTime
                              )
                            val experiencedTravelTime: SimTime = currentSimTime - requestDepartureTime
                            val experiencedEdgeData: List[RouteRequestData.EdgeData] =
                              MATSimRouteOps.convertRouteToEdgeData(experiencedRoute, qSim)
                            val edgeDataRemainingRoute: List[RouteRequestData.EdgeData] =
                              MATSimRouteOps.convertRouteToEdgeData(remaingingRoute, qSim)

                            logger.debug(s"ue agent $personId's matsim-set route: ${fullRoute.mkString("->")}")

                            val thisAgentBatchingData: AgentBatchData =
                              RouteRequestData(
                                request = thisRequest,
                                timeOfRequest = currentSimTime,
                                experiencedTravelTime = experiencedTravelTime,
                                experiencedRoute = experiencedEdgeData,
                                remainingRoute = edgeDataRemainingRoute,
                                remainingRouteDistance = remainingDistance,
                                lastReplanningTime = None
                              )

                            self.newUERouteRequests.prepend(thisAgentBatchingData)
                            self.selfishAgentRoutesAssigned += 1
                            logger.debug(s"added selfish routing request for UE agent $personId")
                        }

                        self.markForUEPathOverwrite.remove(event.getVehicleId)
                      }

                    } else if (!soReplanningThisIteration && self.markForSOPathOverwrite
                                 .isDefinedAt(event.getVehicleId)) {

                      // if we are not replanning, then we want to copy any existing plans for this agent over to MATSim
                      for {
                        personId               <- self.markForSOPathOverwrite.get(event.getVehicleId)
                        mobsimAgent            <- self.qSim.getAgents.asScala.get(personId)
                        leg                    <- MATSimRouteOps.safeGetModifiableLeg(mobsimAgent)
                        completePathsForPerson <- self.completePathStore.get(personId)
                        departureTime          <- DepartureTime.getLegDepartureTime(leg)
                        pathFromPathStore      <- completePathsForPerson.get(departureTime)

                        // checks that there IS a path, and that it's reasonable to assign here
                        if MATSimRouteOps.completePathHasAtLeastTwoLinks(pathFromPathStore)
                      } {
                        // grab stored route and apply it
                        MATSimRouteOps.assignCompleteRouteToLeg(pathFromPathStore, leg)

                        val currentTime: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)
                        logger.debug(
                          s"$currentTime agent $personId: applying stored route with ${pathFromPathStore.length} edges"
                        )

                        self.markForSOPathOverwrite.remove(event.getVehicleId)
                      }
                    }

                  } match {
                    case util.Failure(e) =>
                      logger.error("LinkEnterEvent failure")
                      throw e
                    case util.Success(_) => ()
                  }

                }
              })

              self.qSim.getEventsManager.addHandler(new VehicleLeavesTrafficEventHandler {
                def handleEvent(event: VehicleLeavesTrafficEvent): Unit = {

                  Try {

                    self.reachedDestination += 1

                    val agentId: Id[Person] = event.getPersonId

                    for {
                      mobsimAgent <- self.qSim.getAgents.asScala.get(agentId)
                      leg         <- MATSimRouteOps.getCurrentLegFromPlan(mobsimAgent)
                      // agentHistory <- self
                      agentExperiencedRoute = MATSimRouteOps.convertToCompleteRoute(leg)
                      distance <- MATSimRouteOps.distanceOfPath(agentExperiencedRoute, qSim)
                    } {
                      val departureTime: DepartureTime =
                        self.departureTimeStore.getOrElse(agentId, DepartureTime(Int.MaxValue))

                      // record the agent experience
                      val travelTimeSeconds: Int = event.getTime.toInt - departureTime.value
                      val requestClass: RequestClass =
                        if (self.soAgentReplanningHandler.isUnderControl(agentId)) RequestClass.SO()
                        else RequestClass.UE
                      val replannings: Int = requestClass match {
                        case _: RequestClass.SO =>
                          self.soAgentReplanningHandler.getReplanningCountForAgent(agentId).getOrElse(0)
                        case RequestClass.UE =>
                          if (self.ueAgentAssignedDijkstraRoute(agentId)) 1 else 0
                      }
                      val linestring: String =
                        MATSimRouteToLineString(agentExperiencedRoute, qSim).getOrElse("LINESTRING EMPTY")

                      val agentExperienceRow: String =
                        s"$agentId,$requestClass,$departureTime,$travelTimeSeconds,$distance,$replannings\n"
                      val agentPathRow: String = s"$agentId,$linestring\n"

                      agentExperiencePrintWriter.append(agentExperienceRow)
                      agentPathPrintWriter.append(agentPathRow)

                      // considerations only for SO agents under control
                      if (//!self.onlySelfishAgents &&
                          soReplanningThisIteration &&
                          soAgentReplanningHandler.isUnderControl(agentId)) {

                        // queue up a message to remove this agent from the batching manager

                        val arrivalData = SOAgentArrivalData(
                          agentId = agentId.toString,
                          departureTime = SimTime(departureTime.value),
                          arrivalTime = SimTime(event.getTime.toInt),
                          finalTravelTime = SimTime(travelTimeSeconds),
                          finalDistance = distance
                        )
                        self.agentLeftSimulationRequests += arrivalData

                        // FINALIZE THIS AGENT'S ROUTE FOR NON-PLANNING ITERATIONS
                        logger.debug(s"triggered for so agent $agentId")

                        logger.debug(
                          s"${SimTime(self.playPauseSimulationControl.getLocalTime)} agent $agentId: storing completed route with ${agentExperiencedRoute.length} edges"
                        )

                        logger.debug(
                          s"agent $agentId replanned $replannings times"
                        )

                        // attach this path, keyed by departure time, to the complete list
                        completePathStore.get(agentId) match {
                          case None =>
                            val thisPath = Map(departureTime -> agentExperiencedRoute)
                            completePathStore.update(agentId, thisPath)
                          case Some(alreadyHasPaths) =>
                            completePathStore
                              .update(agentId, alreadyHasPaths.updated(departureTime, agentExperiencedRoute))
                        }
                      }
                    }

                  } match {
                    case util.Failure(e) =>
                      logger.error("VehicleLeavesTrafficEvent failure")
                      throw e
                    case util.Success(_) => ()
                  }

                }
                logger.debug("added person exits vehicle handler")
              })

              self.qSim.addQueueSimulationListeners(new MobsimBeforeSimStepListener {
                override def notifyMobsimBeforeSimStep(e: MobsimBeforeSimStepEvent[_ <: Mobsim]): Unit = {

                  Try {

                    if (soReplanningThisIteration) { // was !self.onlySelfishAgents && soReplanningThisIteration) {

                      // FIND AGENTS FOR REPLANNING AND STORE REQUESTS FOR THEIR ROUTING

                      val nextSimTime = SimTime(e.getSimulationTime.toInt + 1)

                      if (endOfRoutingTime <= nextSimTime) {
                        // noop
                        logger.debug(
                          s"time ${e.getSimulationTime} is beyond end of routing time ${endOfRoutingTime.toString} set in config - no routing will occur"
                        )
                      } else {
                        // construct an SO batch

                        logger.debug(s"finding agents for routing at time ${SimTime(e.getSimulationTime)}")

                        // find the agents who are eligible for re-planning
                        val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
                        val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

                        // convert eligable agents into requests
                        val agentsForReplanning: List[SOAgentReplanningHandler.AgentData] =
                          soAgentReplanningHandler
                            .getActiveAndEligibleForReplanning(currentSimTime)

                        if (agentsForReplanning.isEmpty) {
                          // noop
                          routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)
                          logger.debug(s"at time ${SimTime(e.getSimulationTime)} has no eligible agents for routing")
                        } else {

                          val agentReplanningRequests: List[AgentBatchData] =
                            agentsForReplanning
                              .foldLeft(List.empty[AgentBatchData]) {
                                (mobsimAgents, agentData) =>
                                  {
                                    // get the stateful objects related to this Person/Vehicle
                                    val personId: Id[Person]   = agentData.personId
                                    val vehicleId: Id[Vehicle] = agentData.vehicleId
                                    val agentState = for {
                                      mobsimAgent <- agentsInSimulation.get(personId)
                                      person      <- controler.getScenario.getPopulation.getPersons.asScala.get(personId)
                                      vehicle     <- qSim.getVehicles.asScala.get(vehicleId).map { _.getVehicle }
                                    } yield (mobsimAgent, person, vehicle)

                                    agentState match {
                                      case None =>
                                        logger.debug(
                                          s"agent $agentData that emitted a departure event was not found in sim - possibly already at destination?"
                                        )
                                        mobsimAgents
                                      case Some((mobsimAgent, person, vehicle)) =>
                                        Option(WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)) match {
                                          case None =>
                                            logger.warn(
                                              s"agent $agentData that emitted a departure event does not yet have a trip Leg"
                                            )
                                            mobsimAgents
                                          case Some(leg) =>
                                            // build Requests for this time step
                                            val currentLinkId    = mobsimAgent.getCurrentLinkId
                                            val experiencedRoute = agentData.getExperiencedRoute
                                            val fullRoute        = MATSimRouteOps.convertToCompleteRoute(leg)
                                            val remainingLinkIds = fullRoute.dropWhile(_ != currentLinkId)

                                            val currentLinkDuration = agentData.currentLinkEnterTime
                                              .map { ct =>
                                                self.playPauseSimulationControl.getLocalTime.toLong - ct.value
                                              }
                                              .getOrElse(0L)
                                            val endLinkId         = leg.getRoute.getEndLinkId
                                            val destinationEdgeId = EdgeId(endLinkId.toString)

                                            // get experienced and estimated travel times
                                            val ttRequest = MATSimRouteOps.EdgeDataRequestWithTravelTime(
                                              person,
                                              vehicle,
                                              currentLinkDuration,
                                              self.controler.getLinkTravelTimes()
                                            )
                                            val experiencedEdgeData: List[RouteRequestData.EdgeData] =
                                              MATSimRouteOps
                                                .convertExperiencedRouteToEdgeData(experiencedRoute, qSim)
                                            val remainingEdgeData: List[RouteRequestData.EdgeData] =
                                              MATSimRouteOps.convertRouteToEdgeData(
                                                remainingLinkIds,
                                                qSim,
                                                Some(ttRequest)
                                              )

                                            MATSimRouteOps.selectRequestOriginLink(
                                              fullRoute,
                                              currentLinkId,
                                              endLinkId,
                                              self.qSim,
                                              self.minimumReplanningLeadTime,
                                              self.minimumRemainingRouteTimeForReplanning
                                            ) match {
                                              case None =>
                                                val remainingTT = remainingEdgeData
                                                  .flatMap { _.estimatedTimeAtEdge.map { _.value } }
                                                  .foldLeft(0L) { _ + _ }

                                                logger.debug(
                                                  f"didn't find a reasonable edge to attempt replanning for agent $personId with est. remaining travel time $remainingTT%.2f seconds"
                                                )
                                                soAgentReplanningHandler.incrementNumberFailedRoutingAttempts(personId)
                                                mobsimAgents
                                              case Some(sourceEdgeId) => {
                                                  for {
                                                    departureTime <- soAgentReplanningHandler
                                                      .getDepartureTimeForAgent(personId)
                                                      .map { t => SimTime(t.value) }
                                                  } yield {
                                                    val thisRequest: Request =
                                                      Request(
                                                        agent = personId.toString,
                                                        location = sourceEdgeId,
                                                        destination = destinationEdgeId,
                                                        requestClass = RequestClass.SO(),
                                                        travelMode = TravelMode.Car,
                                                        departureTime = departureTime
                                                      )

                                                    val remainingDistance: Meters =
                                                      MATSimRouteOps
                                                        .distanceOfPath(remainingLinkIds, qSim)
                                                        .getOrElse(Meters.Zero)

                                                    val lastReplanningTime: Option[SimTime] =
                                                      soAgentReplanningHandler
                                                        .getMostRecentTimePlannedForAgent(personId)
                                                    val experiencedTravelTime = currentSimTime - departureTime

                                                    val thisAgentBatchingData: AgentBatchData =
                                                      RouteRequestData(
                                                        request = thisRequest,
                                                        timeOfRequest = currentSimTime,
                                                        experiencedTravelTime = experiencedTravelTime,
                                                        experiencedRoute = experiencedEdgeData,
                                                        remainingRoute = remainingEdgeData,
                                                        remainingRouteDistance = remainingDistance,
                                                        lastReplanningTime = lastReplanningTime
                                                      )

                                                    logger.debug(
                                                      s"requesting route for agent $personId, o=$sourceEdgeId, d=$destinationEdgeId"
                                                    )
                                                    thisAgentBatchingData
                                                  }
                                                } match {
                                                  case None                     => mobsimAgents
                                                  case Some(thisAgentBatchData) => thisAgentBatchData +: mobsimAgents
                                                }

                                            }
                                        }
                                    }
                                  }
                              }

                          // store any agent routing requests
                          if (agentReplanningRequests.isEmpty) {

                            // noop
                            routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)
                            logger.debug(s"at time ${SimTime(e.getSimulationTime)} has no (new) route requests")

                          } else {

                            // construct the payload for this routing request, replacing whatever route request is currently stored
                            val payload = RouteRequests(e.getSimulationTime, agentReplanningRequests)
                            self.newSORouteRequests = Some { payload }
                            routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)
                            logger.debug(
                              s"at time ${SimTime(e.getSimulationTime)} storing ${payload.requests.length} requests for batch route module"
                            )
                          }
                        }
                      }
                    }

                  } match {
                    case util.Failure(e) =>
                      logger.error("MobsimBeforeSimStepEvent failure (route requests)")
                      throw e
                    case util.Success(_) => ()
                  }

                }
                logger.debug("added overriding router as before simstep handler")
              })
            }
          })

      }
    })

    self.matsimState = HandCrankedSimulator.SimulatorState.Initialized
    ()
  }

  /**
    * cranks the simulator forward
    * @return simulator state after one crank
    */
  override def advance(): IO[Unit] = {

    matsimState match {
      case SimulatorState.Initialized =>
        IO {
          logger.debug("initializing MATSim")

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
                logger.error("attempting to activate MATSim in child thread, failed:")
                throw e
            }
          }

          matsimState = SimulatorState.Running

          logger.debug("reached point where simulator can hand off to experiment runner")
          ()
        }

      case SimulatorState.Running =>
        IO {
          // crank matsim

          val iterationBeforeCrank: Int = observedMATSimIteration

          // move one sim step forward, or, if "current time" exceeds end of day (from previous iteration),
          // then advance to zero and collect $200.00.

          val currentTime: SimTime     = SimTime(self.playPauseSimulationControl.getLocalTime)
          val advanceToSimTime: Double = currentTime.value.toDouble + 0.01 //+ SimTime(2) //+ matsimStepSize

          logger.debug(
            s"called on sim in Running state: advancing one time step from $currentTime to $advanceToSimTime"
          )

          self.playPauseSimulationControl.doStep(advanceToSimTime)
          val timeAfterAdvance: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)

          // flush print writers every 15 minutes of sim time
          if (timeAfterAdvance.value > 0 && timeAfterAdvance.value % 900 == 0) {
            self.agentExperiencePrintWriter.flush()
            self.agentPathPrintWriter.flush()
          }

          val matsimFailure: Boolean = self.matsimThreadException.isDefined
          val thisIterationIsFinishedAfterCrank: Boolean =
            self.playPauseSimulationControl.getLocalTime == Double.MaxValue

          if (matsimFailure) {
            for {
              throwable <- self.matsimThreadException
            } {
              logger.error(s"MATSim failed: ${throwable.getMessage}")
              self.matsimState = SimulatorState.Error(throwable.getMessage)
            }
          } else if (thisIterationIsFinishedAfterCrank) {

            observedHitMidnight = true
            logger.debug("hit midnight")

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
                      logger.debug(s"waited $i seconds for time to restart...")
                    }
                  case Failure(e) =>
                    logger.error("waiting for MATSim to start the next iteration, failed:")
                    self.matsimState = SimulatorState.Error(e.getMessage)
                }
              }
            } else {
              logger.debug("hit end of simulation")
              self.matsimState = SimulatorState.Finishing
            }
          }

          // done cranking
          ()
        }

      case x =>
        val exception = new IllegalStateException(s"advance function should never see matsim simulation in state '$x'")
        IO.raiseError(exception)
    }
  }

  /**
    * takes the response from the routing algorithm and modifies MATSim agents with the new routes, unless
    * there is a failure, due to stale information or invalid paths
    * @param responses a list of agent/route pairs
    * @return the simulator state object
    */
  override def assignReplanningRoutes(responses: List[Response]): IO[Unit] = {

    if (responses.isEmpty) {
      logger.debug(s"received 0 route responses - NOOP")
      IO.unit

    } else {

      IO {
        val allResponses: Int = responses.length
        val ueResponses: Int  = responses.count { _.request.requestClass == RequestClass.UE }
        val soResponses: Int  = allResponses - ueResponses
        if (ueResponses > 0) {
          logger.debug(s"received $ueResponses selfish routing responses")
        }
        if (soResponses > 0) {
          logger.debug(s"received $soResponses system optimal route responses")
        }

        val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
        val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

        for {
          response <- responses
          routingResultPath = MATSimRouteOps.convertToMATSimPath(response.path)
          mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))
          mobsimAgentCurrentRouteLinkIdIndex = WithinDayAgentUtils.getCurrentRouteLinkIdIndex(mobsimAgent)
          leg           <- MATSimRouteOps.safeGetModifiableLeg(mobsimAgent)
          departureTime <- DepartureTime.getLegDepartureTime(leg)
          if MATSimRouteOps.confirmPathIsValid(routingResultPath, self.qSim) // todo: report invalid paths
        } {

          // extract the mobsim agent data
          val agentId: Id[Person]                   = mobsimAgent.getId
          val currentLinkId: Id[Link]               = mobsimAgent.getCurrentLinkId
          val agentExperiencedRoute: List[Id[Link]] = MATSimRouteOps.convertToCompleteRoute(leg)

          // combine the old path with the new path
          MATSimRouteOps.coalescePath(agentExperiencedRoute, routingResultPath, currentLinkId) match {
            case Left(reason) =>
              // noop
              logger.warn(
                s"failed - ${response.request.requestClass} agent ${agentId.toString}'s new route at time $currentSimTime not applied due to: $reason"
              )

            case Right(updatedRoute) =>
              // test that the updatedRoute has the currentLinkId at the currentLinkIndex
              // otherwise it would cause this agent to have a bad turn logic error.
              Try { updatedRoute(mobsimAgentCurrentRouteLinkIdIndex) }.toEither match {
                case Left(e) =>
                  logger.warn(
                    s"tried to check that currentLinkIndex contains currentLink in updatedRoute but failed: $e"
                  )
                case Right(updatedRouteLinkAtCurrentLinkIndex) =>
                  if (updatedRouteLinkAtCurrentLinkIndex != currentLinkId) {
                    logger.warn(
                      s"composed a replanning route update but found the currentLinkIndex does not correspond with the currentlinkId"
                    )

                  } else {
                    logger.debug(s"updating route for ${response.request.requestClass} agent ${agentId.toString}")

                    // update the mobsim (takes a complete route, not just the "inner")
                    val route = RouteUtils.createNetworkRoute(updatedRoute.asJava, qSim.getNetsimNetwork.getNetwork)

                    leg.setRoute(route)
                    WithinDayAgentUtils.resetCaches(mobsimAgent)

                    logger.debug(s"replanned route for ${response.request.requestClass} agent $agentId")

                    response.request.requestClass match {
                      case RequestClass.SO(_) =>
                        // continue to record the experienced agent route associated with this departure time
                        completePathStore.get(agentId) match {
                          case None =>
                            completePathStore.update(agentId, Map(departureTime -> updatedRoute)) // in-place
                          case Some(agentAlreadyRecordedInStore) =>
                            completePathStore.update(
                              agentId,
                              agentAlreadyRecordedInStore.updated(departureTime, updatedRoute)
                            ) // in-place
                        }

                        self.soAgentReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentSimTime)
                        self.soAgentReplanningHandler.getReplanningCountForAgent(agentId) match {
                          case None =>
                            logger.error(
                              s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added"
                            )
                          case Some(numberOfPathAssignments) =>
                            logger.debug(
                              s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $currentSimTime"
                            )
                        }
                      case RequestClass.UE =>
                        // record that we assigned this agent a new route plan instead of relied on MATSim's routing
                        ueAgentAssignedDijkstraRoute += agentId
                    }
                  }
              }
          }
        }

        logger.debug(s"modified agents in MATSim based on route responses")

      }
    }
  }

  /**
    * finds what [[SimulatorState]] MATSim is in, or, if is has had a failure
    *
    * @return either an error message, or, a [[HandCrankedSimulator.SimulatorState]] object
    */
  override def getState: IO[SimulatorState] = {

    matsimState match {
      case SimulatorState.Error(msg) =>
        IO.raiseError(new Error(msg))
      case SimulatorState.Finishing =>
        // let's wait until we observe MATSim has finished
        IO {
          val timeoutStop: Long = System.currentTimeMillis + simulationTailTimeout.toMillis

          @tailrec
          def _isDone(): Either[IsDoneFailure, SimulatorState] = {
            if (!t.isAlive) {
              matsimState match {
                case SimulatorState.Error(msg) => Left(IsDoneFailure.FinishingError(msg))
                case SimulatorState.Finishing =>
                  logger.info(s"MATSim Simulation exited normally")
                  Right(SimulatorState.Finished)
                case other =>
                  Left(
                    IsDoneFailure.FinishingError(
                      s"while finishing up simulation, MATSimSimulator went from a ${SimulatorState.Finishing} state to a $other state instead of ${SimulatorState.Finished}"
                    )
                  )
              }
            } else if (System.currentTimeMillis > timeoutStop) {
              Left(
                IsDoneFailure.TimeoutFailure(
                  s"surpassed timeout of ${simulationTailTimeout.toMinutes} minutes waiting for simulation to finish"
                )
              )
            } else {
              Try { Thread.sleep(1000) } match {
                case Success(()) => _isDone()
                case Failure(e) =>
                  Left(
                    IsDoneFailure
                      .TimeoutFailure(s"waiting for MATSim in child thread to terminate, failed: ${e.getStackTrace}")
                  )
              }
            }
          }

          playPauseSimulationControl.play()
          _isDone() match {
            case Left(err) =>
              val msg = s"failed while finishing simulation: $err"
              throw new Error(msg)
            case Right(finishedState) =>
              finishedState
          }
        }
      case SimulatorState.Running if !t.isAlive =>
        matsimState = SimulatorState.Finished
        val error = IsDoneFailure.TimeoutFailure(
          s"MATSim thread is dead but simulation is not finished. MATSim may have had a fatal error."
        )
        IO.raiseError(error)

      case other =>
        IO.pure(other)
    }
  }

  /**
    * gets the road network delta if we have reached an update time. clears the
    * road network delta handler when an update occurs.
    *
    * @return a list of edge id and marginal flow tuples, which may be empty
    */
  override def getUpdatedEdges: IO[List[(EdgeId, Flow)]] = IO {

    val currentTime = SimTime(self.playPauseSimulationControl.getLocalTime)

    // attempt to grab the deltas. empty if we are restricted by this.minNetworkUpdateThreshold.
    // if we aren't, we get the deltas and the RoadNetworkDeltaHandler is reset.
    val updatedEdges: List[(EdgeId, Flow)] = roadNetworkDeltaHandler.getDeltas(currentTime)

    // todo: get zeroes here too!

    updatedEdges
  }

  /**
    * constructs [[Request]] objects for each active agent (SO & UE)
    *
    * @return a list of requests
    */
  override def getAgentsNewlyAvailableForReplanning: IO[List[AgentBatchData]] = IO {
    if (matsimState != SimulatorState.Running) {
      List.empty
    } else {

      val ueRequests: List[AgentBatchData] = self.newUERouteRequests.toList
      self.newUERouteRequests.clear()

      val soBatchData: List[AgentBatchData] = self.newSOAgentBatchData.toList
      self.newSOAgentBatchData.clear()

      val agentsLeftSimulationRequests: List[SOAgentArrivalData] = self.agentLeftSimulationRequests.toList
      self.agentLeftSimulationRequests.clear()

      self.newSORouteRequests match {
        case None =>
          logger.debug(s"returning empty list (no requests)")
          ueRequests ::: soBatchData ::: agentsLeftSimulationRequests
        case Some(rr) =>
          logger.debug(
            s"sending ${rr.requests.size} SO, ${ueRequests.size} UE requests, ${agentsLeftSimulationRequests.size} agents left simulation requests"
          )
          val outgoingRequests: List[AgentBatchData] =
            soBatchData ::: rr.requests ::: ueRequests ::: agentsLeftSimulationRequests
          self.newSORouteRequests = None
          outgoingRequests
      }
    }
  }

  /**
    * returns the SimTime that MATSim is paused at
    * @return a [[SimTime]] object representing time in seconds
    */
  override def getCurrentSimTime: IO[SimTime] = IO {
    val currentTime = SimTime(self.playPauseSimulationControl.getLocalTime)
    logger.debug(s"$currentTime")
    currentTime
  }
}
