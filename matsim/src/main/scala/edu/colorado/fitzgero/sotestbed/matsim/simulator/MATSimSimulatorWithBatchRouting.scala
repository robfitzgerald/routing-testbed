package edu.colorado.fitzgero.sotestbed.matsim.simulator

import java.io.PrintWriter
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import cats.effect.IO
import cats.effect.unsafe.implicits.global // used within MATSim extensions for IO blocks
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.{MATSimConfig, MATSimRunConfig}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator
import edu.colorado.fitzgero.sotestbed.simulator.HandCrankedSimulator.SimulatorState
import edu.colorado.fitzgero.sotestbed.matsim.simulator.GenerateAgentData
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
import org.matsim.core.router.util.TravelTime
import edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler.RoadNetworkFlowHandler

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
  var minimumNetworkUpdateThreshold: SimTime                    = _
  var simulationTailTimeout: Duration                           = _

  // simulation state variables
  var matsimState: HandCrankedSimulator.SimulatorState = HandCrankedSimulator.SimulatorState.Uninitialized
  var matsimOverridingModuleAdded: Boolean             = false
  var observedMATSimIteration: Int                     = 0
  var observedHitMidnight: Boolean                     = false
  var soReplanningThisIteration: Boolean               = false
  var reachedDestination: Int                          = 0
  var selfishAgentRoutesAssigned: Int                  = 0
  var matsimShutdown: Boolean                          = false

  // simulation state containers and handlers
  var roadNetworkFlowHandler: RoadNetworkFlowHandler     = _
  var soAgentReplanningHandler: SOAgentReplanningHandler = _

  val completePathStore: collection.mutable.Map[Id[Person], Map[DepartureTime, List[Id[Link]]]] =
    collection.mutable.Map.empty
  val departureTimeStore: collection.mutable.Map[Id[Person], DepartureTime]              = collection.mutable.Map.empty
  val markForSOPathOverwrite: collection.mutable.Map[Id[Vehicle], Id[Person]]            = collection.mutable.Map.empty
  val markForUEPathOverwrite: collection.mutable.Map[Id[Vehicle], (Id[Person], SimTime)] = collection.mutable.Map.empty
  val newSOAgentBatchData: collection.mutable.ListBuffer[AgentBatchData]                 = collection.mutable.ListBuffer.empty
  val ueAgentAssignedDijkstraRoute: collection.mutable.Set[Id[Person]]                   = collection.mutable.Set.empty

  // logging
  var runStatsPrintWriter: PrintWriter        = _
  var agentExperiencePrintWriter: PrintWriter = _
  var agentPathPrintWriter: PrintWriter       = _

  // matsim variables
  var controler: Controler                                      = _
  var qSim: QSim                                                = _
  var playPauseSimulationControl: PlayPauseControlSecondStepper = _
  var t: Thread                                                 = _
  var matsimThreadException: Option[Throwable]                  = None
  var travelTimeCalculator: TravelTimeCalculator                = _

  /**
    * grab the current sim time according to MATSim. calling this method whenever checking
    * the time ensures we are always up-to-date with MATSim. the value of the current time
    * changes within the advance method of this trait; it is especially important to invoke
    * this method rather than store it's result within the logic of that method.
    */
  def currentTime: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)

  /**
    * initializes MATSim and registers handlers/listeners which store stateful information via mutable
    * semantics in the scope of [[MATSimSimulatorWithBatchRouting]]
    *
    * @param config configuration relevant to this simulator
    * @return the simulator state object, which is nothing, since all ops are side effects
    */
  override def initializeSimulator(config: SimulatorConfiguration): IO[Unit] =
    IO.fromTry(Try {

      logger.debug("MATSimProxy.initializeSimulator")

      // MATSimProxy config
      self.endOfRoutingTime = config.run.endOfRoutingTime
      self.batchWindow = config.routing.batchWindow
      self.matsimStepSize = config.run.matsimStepSize
      self.matsimSemaphoreTimeoutMs = config.run.matsimSemaphoreTimeoutMs
      self.maxPathAssignments = config.routing.maxPathAssignments
      self.minimumReplanningLeadTime = config.routing.minimumReplanningLeadTime
      self.minimumRemainingRouteTimeForReplanning = config.routing.minimumReplanningLeadTime
      self.minimumNetworkUpdateThreshold = config.routing.minNetworkUpdateThreshold
      self.simulationTailTimeout = config.run.simulationTailTimeout

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

      // needs to happen after the controler checks the experiment directory (20200125-is this still true?)
      val statsFilePath: String =
        config.experimentLoggingDirectory.resolve(s"stats-${config.algorithm.name}.txt").toString
      runStatsPrintWriter = new PrintWriter(statsFilePath)
      runStatsPrintWriter.write(s"experiment $experimentPath\n\n")

      val agentExperienceFilePath: String = config.experimentLoggingDirectory.resolve(s"agentExperience.csv").toString
      agentExperiencePrintWriter = new PrintWriter(agentExperienceFilePath)
      agentExperiencePrintWriter.write(
        "agentId,requestClass,departureTime,travelTime,freeFlowTravelTime,distance,replannings\n"
      )

      val agentPathFilePath: String = config.experimentLoggingDirectory.resolve(s"agentPath.csv").toString
      agentPathPrintWriter = new PrintWriter(agentPathFilePath)
      agentPathPrintWriter.write("agentId,WKT\n")

      // initialize intermediary data structures holding data between route algorithms + simulation
      self.soAgentReplanningHandler = new SOAgentReplanningHandler(
        config.agentsUnderControl,
        config.routing.maxPathAssignments,
        config.routing.minimumReplanningWaitTime
      )
      self.roadNetworkFlowHandler = new RoadNetworkFlowHandler

      // track iterations in MATSimProxy
      self.controler.addControlerListener(new IterationStartsListener {
        def notifyIterationStarts(event: IterationStartsEvent): Unit = {
          logger.debug(s"beginning iteration ${event.getIteration}")

          self.observedMATSimIteration = event.getIteration
          self.observedHitMidnight = false
          self.reachedDestination = 0
          self.matsimShutdown = false

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
          self.roadNetworkFlowHandler.clear()

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

          Try {

            logger.info(s"begin ending iteration ${event.getIteration}")

            val iterationPrefix: String = s"it-${event.getIteration}"
            val avgPaths: Double        = soAgentReplanningHandler.getAvgPathsAssigned
            val avgFailed: Double       = soAgentReplanningHandler.getAvgFailedRoutingAttempts
            val sumFailed: Int          = soAgentReplanningHandler.getSumFailedRoutingAttempts

            runStatsPrintWriter.append(s"$iterationPrefix.sum.selfish.assignments = $selfishAgentRoutesAssigned\n")
            runStatsPrintWriter.append(s"$iterationPrefix.avg.paths.assigned = $avgPaths\n")
            runStatsPrintWriter.append(s"$iterationPrefix.avg.failed.routing.attempts = $avgFailed\n")
            runStatsPrintWriter.append(s"$iterationPrefix.sum.failed.routing.attempts = $sumFailed\n")
            runStatsPrintWriter.append(s"$iterationPrefix.sum.reached_destination = ${self.reachedDestination}\n")

          } match {
            case Failure(exception) =>
              throw new Error(s"MATSim IterationEndsListener failure", exception)
            case Success(_) =>
              logger.info(s"finished ending iteration ${event.getIteration}")
          }
        }

        def notifyShutdown(shutdownEvent: ShutdownEvent): Unit = {
          Try {
            runStatsPrintWriter.close()
            agentExperiencePrintWriter.close()
            agentPathPrintWriter.close()
            self.matsimShutdown = true
          } match {
            case Failure(exception) =>
              throw new Error(s"failure closing log files at shutdown", exception)
            case Success(_) =>
              logger.info("finished closing logging files")
          }
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
                // must wait until MATSim is running to add these extensions
                self.playPauseSimulationControl = new PlayPauseControlSecondStepper(self.qSim)
                self.playPauseSimulationControl.pause()

                self.travelTimeCalculator =
                  new TravelTimeCalculator.Builder(self.qSim.getNetsimNetwork.getNetwork).build()
                // this is called at the top of every iteration, so we must check to make sure
                // we add exactly one version of each listeners/handler to the qSim
                if (!matsimOverridingModuleAdded) {

                  self.qSim.getEventsManager.addHandler(self.soAgentReplanningHandler)
                  self.qSim.getEventsManager.addHandler(self.roadNetworkFlowHandler)
                  self.qSim.getEventsManager.addHandler(self.travelTimeCalculator)

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

                        self.markForUEPathOverwrite.update(vehicleId, (personId, simTime))
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
                        val generateArgs = GenerateAgentData.GenerateEnterSimulation(
                          self.travelTimeCalculator.getLinkTravelTimes,
                          personId,
                          vehicleId,
                          simTime
                        )
                        val msg = GenerateAgentData
                          .generateEnterSimulation(self.qSim, generateArgs)
                          .unsafeRunSync
                        self.newSOAgentBatchData.prepend(msg)

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

                      val vehicleId = event.getVehicleId

                      // remove marks for UE agents that received their route instructions
                      self.markForUEPathOverwrite.get(vehicleId).foreach {
                        case (personId, _) =>
                          val wasAssigned = self.ueAgentAssignedDijkstraRoute(personId)
                          if (wasAssigned) self.markForUEPathOverwrite.remove(vehicleId)
                      }

                      // if we are affixing routes for SO agents, apply the stored route
                      if (!soReplanningThisIteration) {
                        // extract stored route and apply it to this agent, then remove the mark
                        self.markForSOPathOverwrite.get(vehicleId).foreach { personId =>
                          GenerateAgentDataOps
                            .applyStoredRoute(qSim, vehicleId, personId, self.completePathStore)
                            .unsafeRunSync
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
                        agentExperiencedRoute = MATSimRouteOps.convertToCompleteRoute(leg)
                        distance <- MATSimRouteOps.distanceOfMATSimPath(agentExperiencedRoute, qSim)
                        ffTravelTimes = MATSimRouteOps.freeFlowTravelTime(agentExperiencedRoute, qSim)
                      } {

                        val departureTime: DepartureTime =
                          self.departureTimeStore
                            .getOrElse(
                              agentId,
                              throw new Error(s"agent missing from departure time store")
                            )
                        self.departureTimeStore.remove(agentId)

                        // record the agent experience
                        val travelTimeSeconds: Int = event.getTime.toInt - departureTime.value
                        val freeFlowTravelTime     = ffTravelTimes.foldLeft(0.0) { case (acc, (_, t)) => acc + t }
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
                          s"$agentId,$requestClass,$departureTime,$travelTimeSeconds,$freeFlowTravelTime,$distance,$replannings\n"
                        val agentPathRow: String = s"$agentId,$linestring\n"

                        agentExperiencePrintWriter.append(agentExperienceRow)
                        agentPathPrintWriter.append(agentPathRow)

                        // considerations only for SO agents under control
                        if (soReplanningThisIteration &&
                            soAgentReplanningHandler.isUnderControl(agentId)) {

                          // queue up a message to remove this agent from the batching manager

                          val arrivalData = SOAgentArrivalData(
                            agentId = agentId.toString,
                            requestClass = requestClass,
                            departureTime = SimTime(departureTime.value),
                            arrivalTime = SimTime(event.getTime.toInt),
                            finalTravelTime = SimTime(travelTimeSeconds),
                            finalDistance = distance,
                            finalFreeFlowTravelTime = SimTime(freeFlowTravelTime)
                          )
                          self.newSOAgentBatchData.prepend(arrivalData)
                          // self.agentLeftSimulationRequests += arrivalData

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
              }
            })

        }
      })

      self.matsimState = HandCrankedSimulator.SimulatorState.Initialized
      ()
    })

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

          // val currentTime: SimTime     = SimTime(self.playPauseSimulationControl.getLocalTime)
          // a little magic to attempt to synchronize to the step size that the configuration specified and to not fall off
          // roughly, the problem here is that MATSim is entirely event-driven and so there needs to be data on each time step
          // in order to "land" on it from an external stepping mechanism. that, and, the timing of "doStep" doesn't
          // seem straight-forward. to get reliable increments of 30 seconds, we need to
          // 1. add the requested step size
          // 2. compute any seconds we would be requesting over the requested self.matsimStepSize and remove them
          // 3. remove 0.99 from this; strangely, we get a more reliable result if we request 0.01 seconds after the
          //    previous second than if we request landing on the exact second set by our self.matsimStepSize.
          val rawStep = currentTime + self.matsimStepSize
          val overage = rawStep % self.matsimStepSize
          val increment =
            if (self.matsimStepSize.value <= 1) 0.01
            else (self.matsimStepSize - overage).value.toDouble - 0.99
          val advanceToSimTime: Double = currentTime.value.toDouble + increment //+ SimTime(2) //+ matsimStepSize

          logger.debug(
            s"called on sim in Running state: advancing one time step from $currentTime to ${SimTime(advanceToSimTime)}"
          )

          self.playPauseSimulationControl.doStep(advanceToSimTime)
          // val timeAfterAdvance: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)

          // flush print writers every 15 minutes of sim time
          if (currentTime.value > 0 && currentTime.value % 900 == 0) {
            self.agentExperiencePrintWriter.flush()
            self.agentPathPrintWriter.flush()
          }

          val matsimFailure: Boolean                     = self.matsimThreadException.isDefined
          val thisIterationIsFinishedAfterCrank: Boolean = currentTime.value == Double.MaxValue

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
              while (stillAlive && currentTime.value >= SimTime.EndOfDay.value) {
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
        // val currentSimTime: SimTime                          = SimTime(currentTime)

        for {
          response <- responses
          routingResultPath = MATSimRouteOps.convertToMATSimPath(response.path)
          mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))
          mobsimAgentCurrentRouteLinkIdIndex = WithinDayAgentUtils.getCurrentRouteLinkIdIndex(mobsimAgent)
          leg           <- MATSimRouteOps.safeGetModifiableLeg(mobsimAgent)
          departureTime <- DepartureTime.getLegDepartureTime(leg)
        } {

          MATSimRouteOps.findErrorsInPath(routingResultPath, self.qSim) match {
            case Some(invalidLinks) =>
              val msg = "route returned from routing algorithm is not a fully-connected path. " +
                "invalid link pairs are shown here: " +
                invalidLinks.map { case (s, d) => f"-[$s]> --/-> -[$d]>" }.mkString(", ")
              logger.warn(msg)

            case None =>
              // extract the mobsim agent data
              val agentId: Id[Person]     = mobsimAgent.getId
              val currentLinkId: Id[Link] = mobsimAgent.getCurrentLinkId
              // mobsimAgent
              val agentExperiencedRoute: List[Id[Link]] = MATSimRouteOps.convertToCompleteRoute(leg)

              // combine the old path with the new path
              MATSimRouteOps.coalescePath(agentExperiencedRoute, routingResultPath, currentLinkId) match {
                case Left(reason) =>
                  // noop
                  logger.warn(
                    s"failed - ${response.request.requestClass} agent ${agentId.toString}'s new route at time $currentTime not applied due to: $reason"
                  )
                  self.soAgentReplanningHandler.incrementNumberFailedRoutingAttempts(agentId)
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

                            self.soAgentReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentTime)
                            self.soAgentReplanningHandler.getReplanningCountForAgent(agentId) match {
                              case None =>
                                logger.error(
                                  s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added"
                                )
                              case Some(numberOfPathAssignments) =>
                                logger.debug(
                                  s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $currentTime"
                                )
                            }
                          case RequestClass.UE =>
                            // record that we assigned this agent a new route plan instead of relied on MATSim's routing
                            self.ueAgentAssignedDijkstraRoute += agentId
                            self.selfishAgentRoutesAssigned += 1
                            logger.debug(s"added selfish routing request for UE agent $agentId")
                          // note: we then need to remove the mark that this UE agent required
                          // a re-route, but we don't have the Id[Vehicle] in scope here nor do
                          // we have a way to retrieve one (!) so instead, that must happen where
                          // UE route requests are generated.

                        }
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
                    .TimeoutFailure(s"waiting for MATSim in child thread to terminate, failed", e)
                )
            }
          }
        }

        playPauseSimulationControl.play()
        _isDone() match {
          case Left(err) =>
            val msg = s"failed while finishing simulation"
            IO.raiseError(new Error(msg, err))
          case Right(finishedState) =>
            IO.pure(finishedState)
        }

      case SimulatorState.Running if self.matsimShutdown =>
        // looks like we got out of sync
        self.matsimState = SimulatorState.Finished
        IO.pure(SimulatorState.Finished)

      case SimulatorState.Running if !t.isAlive && !self.matsimShutdown =>
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
  override def getUpdatedEdges: IO[List[(EdgeId, Option[Flow], MetersPerSecond)]] = IO {

    val binStartTime = SimTime(this.playPauseSimulationControl.getLocalTime) - this.minimumNetworkUpdateThreshold

    // collects the current flow count and average trip duration travel time for each link
    // and then resets the observations for the next window of time
    val updates = this.roadNetworkFlowHandler.collectObservations(binStartTime)

    val tt = self.travelTimeCalculator.getLinkTravelTimes
    val rows = this.qSim.getNetsimNetwork.getNetwork.getLinks.values.asScala.toList.map {
      case link: Link =>
        val linkId = link.getId
        val edgeId = EdgeId(linkId.toString)
        val obs    = updates.get(linkId)
        val travelTime = obs
          .flatMap(_.averageTraversalDurationSeconds)
          .getOrElse { math.max(1.0, link.getLength / link.getFreespeed) }
        // val travelTime = MATSimRouteOps.getLinkTravelTime(tt, link, currentTime)
        val speed = MetersPerSecond(Meters(link.getLength.toLong), TravelTimeSeconds(travelTime))
        val flow  = obs.map { o => Flow(o.flowCount) }
        (edgeId, flow, speed)
    }

    rows
  }

  /**
    * constructs [[AgentBatchData.RouteRequestData]] objects for each eligible agent (SO & UE)
    *
    * @return a list of requests
    */
  override def getAgentsNewlyAvailableForReplanning: IO[List[AgentBatchData]] = {

    val agentBatchDataResult = Try {
      // generate requests for UE agents
      val ueRequestFn: (Id[Vehicle], Id[Person], SimTime) => IO[Option[AgentBatchData]] =
        GenerateAgentDataOps.generateUeAgentRouteRequest(
          self.qSim,
          self.travelTimeCalculator.getLinkTravelTimes,
          self.minimumReplanningLeadTime,
          self.minimumRemainingRouteTimeForReplanning
        )
      val ueRequestsResult: IO[List[AgentBatchData]] = self.markForUEPathOverwrite.toList
        .traverse {
          case (vId, (pId, simTime)) =>
            ueRequestFn(vId, pId, simTime).map { r => (vId, r) }
        }
        .map { reqs =>
          reqs.foreach {
            case (vId, Some(_)) =>
              // this should really be removed when we receive and apply a route revision
              // but instead it's here where we have the vehicle id in scope.. :-|
              self.markForUEPathOverwrite.remove(vId)
            case _ =>
            // NOOP
          }
          reqs.flatMap { case (vId, req) => req }
        }

      // we only generate requests for SO agents if these condititions are true
      val nextSimTime  = SimTime(currentTime.value + 1)
      val isRunning    = matsimState == SimulatorState.Running
      val isNotTooLate = nextSimTime < self.endOfRoutingTime

      // generate requests for SO agents that are active and eligible
      // for replanning based on the simulation configuration
      val soRequestFn: AgentData => IO[Option[AgentBatchData]] =
        GenerateAgentDataOps.generateSoAgentRouteRequest(
          self.qSim,
          self.soAgentReplanningHandler,
          self.travelTimeCalculator.getLinkTravelTimes,
          self.minimumReplanningLeadTime,
          self.minimumRemainingRouteTimeForReplanning
        )
      val soRequestsResult = if (!(self.soReplanningThisIteration && isRunning && isNotTooLate)) {
        IO.pure(List.empty[AgentBatchData])
      } else {
        self.soAgentReplanningHandler
          .getActiveAndEligibleForReplanning(currentTime)
          .traverse(soRequestFn)
          .map { _.flatten }
      }

      // combine UE and SO agent messages and return
      val result = for {
        ueReqs <- ueRequestsResult
        soReqs <- soRequestsResult
      } yield {
        // append other SO messages reporting departure and arrival. these can conflict
        // with one another, so make sure we are sending only the correct combination of messages
        // in the correct order
        val soMsgs = (soReqs ::: newSOAgentBatchData.toList)
          .groupBy(_.agent)
          .flatMap { case (_, msgs) => AgentBatchData.prepareMessagesForRoutingServer(msgs) }
          .toList
        newSOAgentBatchData.clear()

        logger.debug(s"sending ${soReqs.length} SO msgs, ${ueReqs.length} UE msgs to routing server")
        ueReqs ::: soMsgs
      }

      result
    }

    IO.fromTry(agentBatchDataResult).flatten
  }

  /**
    * returns the SimTime that MATSim is paused at
    * @return a [[SimTime]] object representing time in seconds
    */
  override def getCurrentSimTime: IO[SimTime] = IO {
    currentTime
  }
}
