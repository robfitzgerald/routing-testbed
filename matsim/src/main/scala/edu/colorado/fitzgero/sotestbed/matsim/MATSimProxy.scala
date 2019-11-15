package edu.colorado.fitzgero.sotestbed.matsim

import java.nio.file.{Files, Path}
import java.util.concurrent.{Semaphore, TimeUnit}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import cats.effect.SyncIO
import org.apache.log4j.{Level, Logger}

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, Response, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps
import edu.colorado.fitzgero.sotestbed.simulator.SimulatorOps.SimulatorState
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.events.handler.{VehicleEntersTrafficEventHandler, VehicleLeavesTrafficEventHandler}
import org.matsim.api.core.v01.events.{VehicleEntersTrafficEvent, VehicleLeavesTrafficEvent}
import org.matsim.api.core.v01.network.Link
import org.matsim.api.core.v01.population.{Leg, Person}
import org.matsim.core.config._
import org.matsim.core.controler.events.IterationStartsEvent
import org.matsim.core.controler.listener.IterationStartsListener
import org.matsim.core.controler.{AbstractModule, Controler}
import org.matsim.core.mobsim.framework.events.{MobsimBeforeSimStepEvent, MobsimInitializedEvent}
import org.matsim.core.mobsim.framework.listeners.{MobsimBeforeSimStepListener, MobsimInitializedListener}
import org.matsim.core.mobsim.framework.{Mobsim, MobsimAgent, PlayPauseSimulationControl}
import org.matsim.core.mobsim.qsim.QSim
import org.matsim.core.mobsim.qsim.agents.WithinDayAgentUtils
import org.matsim.core.population.routes.{NetworkRoute, RouteUtils}

/**
  * performs [[SimulatorOps]] on a MATSim simulation which allows it to be used in a [[edu.colorado.fitzgero.sotestbed.experiment.RoutingExperiment]]
  *
  */
trait MATSimProxy extends SimulatorOps[SyncIO] with LazyLogging { self =>

  override type Simulator              = MATSimSimulation
  override type SimulatorConfiguration = MATSimRunConfig

  // configuration-based variables
  var endOfRoutingTime: SimTime                                 = _
  var lastIteration: Int                                        = _
  var batchWindow: SimTime = _
  var matsimStepSize: SimTime                                   = _
  var maxPathAssignments: Int                                   = _
  var reasonableReplanningLeadTime: TravelTimeSeconds           = _
  var minimumRemainingRouteTimeForReplanning: TravelTimeSeconds = _
  var simulationTailTimeout: Duration                           = _

  // simulation state variables
  var matsimState: SimulatorOps.SimulatorState  = SimulatorOps.SimulatorState.Uninitialized
  var observedMATSimIteration: Int              = 0
  var observedHitMidnight: Boolean              = false
  var routingRequestsUpdatedToTimeStep: SimTime = SimTime.Zero
  var soReplanningThisIteration: Boolean        = false

  // simulation state containers and handlers
  var newRouteRequests: Option[MATSimProxy.RouteRequests]                                                   = None
  var roadNetworkDeltaHandler: RoadNetworkDeltaHandler                                                      = _
  var agentsInSimulationNeedingReplanningHandler: AgentsInSimulationNeedingReplanningHandler                = _
  val completePathStore: collection.mutable.Map[Id[Person], Map[MATSimProxy.DepartureTime, List[Id[Link]]]] = collection.mutable.Map.empty

  // matsim variables
  var controler: Controler                                   = _
  var qSim: QSim                                             = _
  var playPauseSimulationControl: PlayPauseSimulationControl = _
  var t: Thread                                              = _

  // support methods
  def canJoinBatch(currentTime: SimTime, batchEndTime: SimTime): Boolean = currentTime < batchEndTime - batchWindow

  override def initializeSimulator(config: SimulatorConfiguration): SyncIO[MATSimSimulation] = SyncIO {

    logger.debug("MATSimProxy.initializeSimulator")

    // MATSimProxy config
    endOfRoutingTime = config.run.endOfRoutingTime
    lastIteration = config.run.lastIteration
    batchWindow = config.routing.batchWindow
    matsimStepSize = config.run.matsimStepSize
    maxPathAssignments = config.routing.maxPathAssignments
    reasonableReplanningLeadTime = config.routing.reasonableReplanningLeadTime
    minimumRemainingRouteTimeForReplanning = config.routing.reasonableReplanningLeadTime
    simulationTailTimeout = config.run.simulationTailTimeout

    // file system configuration
    val experimentPath: Path =
      config.io.workingDirectory.resolve(config.io.experimentSubdirectoryName)
    Files.createDirectories(experimentPath)

    // matsim configuration
    Logger.getLogger("org.matsim").setLevel(Level.toLevel(config.io.matsimLogLevel))
    val matsimConfig: Config = ConfigUtils.loadConfig(config.io.matsimConfigFile.toString)
    matsimConfig.controler.setOutputDirectory(experimentPath.toString)
    matsimConfig.plans.setInputFile(config.io.populationFile.toString)
    matsimConfig.network.setInputFile(config.io.matsimNetworkFile.toString)
    matsimConfig.controler.setLastIteration(config.run.lastIteration)

    // start MATSim and capture object references to simulation in broader MATSimActor scope
    controler = new Controler(matsimConfig)

    // initialize intermediary data structures holding data between route algorithms + simulation
    agentsInSimulationNeedingReplanningHandler = new AgentsInSimulationNeedingReplanningHandler(
      config.pop.agentsUnderControl,
      config.routing.minimumReplanningWaitTime,
      config.routing.maxPathAssignments
    )
    roadNetworkDeltaHandler = new RoadNetworkDeltaHandler()

    // track iterations in MATSimProxy
    controler.addControlerListener(new IterationStartsListener {
      def notifyIterationStarts(event: IterationStartsEvent): Unit = {
        logger.debug(s"beginning iteration ${event.getIteration}")

        observedMATSimIteration = event.getIteration
        observedHitMidnight = false

        agentsInSimulationNeedingReplanningHandler.clear()
        roadNetworkDeltaHandler.clear()

        soReplanningThisIteration = if (config.run.soRoutingIterationCycle == 0) {
          true // prevent divide-by-zero; soRoutingIterationCycle == 0 => so-routing every iteration
        } else if (event.getIteration == 0) {
          true // always perform so-routing on first iteration (invariant for soReplanningThisIteration==false iterations)
        } else {
          event.getIteration % config.run.soRoutingIterationCycle == 0
        }
      }
    })

    // inject handlers and listeners for MATSim integration
    controler.addOverridingModule(new AbstractModule() { module =>
      @Override def install(): Unit = {

        module
          .addMobsimListenerBinding()
          .toInstance(new MobsimInitializedListener {

            def notifyMobsimInitialized(e: MobsimInitializedEvent[_ <: Mobsim]): Unit = {
              // grab the QSim once it has been initialized so we can add modules to it
              self.qSim = e.getQueueSimulation.asInstanceOf[QSim]

              // start the playPause functionality
              self.playPauseSimulationControl = new PlayPauseSimulationControl(self.qSim)
              self.playPauseSimulationControl.pause()

              // track active agents under control
              self.qSim.getEventsManager.addHandler(agentsInSimulationNeedingReplanningHandler)
              self.qSim.getEventsManager.addHandler(roadNetworkDeltaHandler)

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
                    completePathStore.remove(event.getPersonId)

                  } else if (agentsInSimulationNeedingReplanningHandler.isUnderControl(event.getPersonId)) {
                    logger.debug(s"[VehicleEntersTrafficEventHandler] triggered for so agent ${event.getPersonId}")

                    // ITERATION WITHOUT REPLANNING: OVERWRITE THIS AGENT'S ROUTE FOR THIS
                    // DEPARTURE TIME FROM THE COMPLETE PATH STORE

                    for {
                      mobsimAgent <- qSim.getAgents.asScala.get(event.getPersonId)
                      leg = WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)
                      pathFromPathStore = completePathStore
                        .getOrElse(event.getPersonId, Map.empty)
                        .getOrElse(MATSimProxy.DepartureTime(leg.getDepartureTime.toInt), List.empty)
                      if MATSimProxy.pathHasAtLeastTwoLinks(pathFromPathStore)
                    } {

                      // grab stored route and apply it

                      logger.debug(
                        s"[VehicleEntersTrafficEventHandler] ${SimTime(self.playPauseSimulationControl.getLocalTime)} agent ${event.getPersonId}: applying stored route with ${pathFromPathStore.length} edges")

                      MATSimProxy.assignCompleteRouteToLeg(pathFromPathStore, leg)
                    }
                  }
                }
                logger.debug("added person enters vehicle handler")
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
                      plan = WithinDayAgentUtils.getModifiablePlan(mobsimAgent)
                      leg <- plan.getPlanElements.asScala.toList
                        .find { l =>
                          l.isInstanceOf[Leg] && l.asInstanceOf[Leg].getDepartureTime.toInt == departureTime.value
                        }
                        .map { _.asInstanceOf[Leg] }
                      val agentExperiencedRoute = MATSimProxy.convertToCompleteRoute(leg)
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
                          val thisPath: Map[MATSimProxy.DepartureTime, List[Id[Link]]] =
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

                      // get the changes to the road network observed since last sim step
                      val networkDeltas: Map[EdgeId, Int] = roadNetworkDeltaHandler.getDeltasAsEdgeIds
                      roadNetworkDeltaHandler.clear()

                      // find the agents who are eligible for re-planning
                      val agentsInSimulation: Map[Id[Person], MobsimAgent] = self.qSim.getAgents.asScala.toMap
                      val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

                      // convert eligable agents into requests
                      val agentsForReplanning: List[Request] =
                        agentsInSimulationNeedingReplanningHandler
                          .getActiveAndEligibleForReplanning(currentSimTime)
                          .foldLeft(List.empty[Request]) {
                            (mobsimAgents, agentInSimulation) =>
                              agentsInSimulation.get(agentInSimulation) match {
                                case None =>
                                  logger.error(
                                    s"[MobsimBeforeSimStepListener] agent $agentInSimulation that emitted a departure event was not found in QSim")
                                  mobsimAgents
                                case Some(mobsimAgent) =>
                                  val agentId           = mobsimAgent.getId
                                  val leg               = WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent)
                                  val fullRoute         = MATSimProxy.convertToCompleteRoute(leg)
                                  val currentLinkId     = mobsimAgent.getCurrentLinkId
                                  val destinationLinkId = leg.getRoute.getEndLinkId
                                  val destinationEdgeId = EdgeId(destinationLinkId.toString)
                                  MATSimProxy.selectRequestOriginLink(fullRoute,
                                                                      currentLinkId,
                                                                      destinationLinkId,
                                                                      self.qSim,
                                                                      reasonableReplanningLeadTime,
                                                                      minimumRemainingRouteTimeForReplanning) match {
                                    case None =>
                                      logger.error(
                                        s"[MobsimBeforeSimStepListener] didn't find a reasonable edge to attempt replanning for agent $agentId")
                                      mobsimAgents
                                    case Some(sourceEdgeId) =>
                                      // count this replanning request
                                      agentsInSimulationNeedingReplanningHandler.incrementAgentDataDueToReplanning(agentId, currentSimTime)

                                      val thisRequest: Request =
                                        Request(
                                          agentId.toString,
                                          sourceEdgeId,
                                          destinationEdgeId,
                                          RequestClass.SO(),
                                          TravelMode.Car
                                        )

                                      logger.debug(
                                        s"[MobsimBeforeSimStepListener] requesting route for agent $agentId, o=$sourceEdgeId, d=$destinationEdgeId")
                                      thisRequest +: mobsimAgents
                                  }
                              }
                          }

                      // store any agent routing requests
                      if (agentsForReplanning.nonEmpty) {
                        // construct the payload for this routing request
                        val payload: MATSimProxy.RouteRequests =
                          MATSimProxy.RouteRequests(
                            e.getSimulationTime,
                            agentsForReplanning,
                            networkDeltas
                          )

                        // set aside payload of agents to route
                        newRouteRequests match {
                          case None =>
                            newRouteRequests = Some { payload }
                          case Some(alreadyHasAPayload) =>
                            val resolvedExistingWithNewRequests = MATSimProxy.resolvePayloads(alreadyHasAPayload, payload)
                            logger.debug(
                              s"[MobsimBeforeSimStepListener] ${SimTime(self.playPauseSimulationControl.getLocalTime)} updated requests count to ${resolvedExistingWithNewRequests.requests.size}")
                            newRouteRequests = Some { resolvedExistingWithNewRequests }
                        }
                        routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)

                        logger.debug(
                          s"[MobsimBeforeSimStepListener] at time ${SimTime(e.getSimulationTime)} storing ${payload.requests.length} requests for batch route module")
                      } else {

                        // do nothing
//                          newRouteRequests match {
//                            case None =>
//                              newRouteRequests = None
//                            case Some(_) =>
//                              // noop
//                          }
                        routingRequestsUpdatedToTimeStep = SimTime(e.getSimulationTime)

                        logger.debug(s"[MobsimBeforeSimStepListener] at time ${SimTime(e.getSimulationTime)} has no (new) route requests")
                      }
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

  override def advance(simulator: Simulator): SyncIO[Simulator] = SyncIO {

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
        val access: Semaphore = self.playPauseSimulationControl.getAccess
        // blocks until doStep is completed.
        val startWait: Long = System.currentTimeMillis
        access.tryAcquire(5, TimeUnit.SECONDS)
        access.release()

        val timeAfterAdvance: SimTime = SimTime(self.playPauseSimulationControl.getLocalTime)
        val waitDuration: String      = f"${(System.currentTimeMillis - startWait).toDouble / 1000.0}%.2f"

        logger.debug(
          s"[advance] advanced from $currentTime to $timeAfterAdvance (${timeAfterAdvance - currentTime} seconds) observed in $waitDuration seconds runtime")

        val thisIterationIsFinishedAfterCrank: Boolean = self.playPauseSimulationControl.getLocalTime == Double.MaxValue

        if (thisIterationIsFinishedAfterCrank) {

          observedHitMidnight = true
          logger.debug("[advance] hit midnight")

          if (iterationBeforeCrank < lastIteration) {
            // we are already on the next iteration - possibly last iteration - though time hasn't restarted.
            // since we aren't on the last iteration, then we should expect time to come around
            // so, let's wait for that to be true.
            var i: Int = 0
            while (self.playPauseSimulationControl.getLocalTime >= SimTime.EndOfDay.value) {
              Try {
                Thread.sleep(1000)
              } match {
                case Success(()) =>
                  i = i + 1
                  logger.debug(s"[advance] waited $i seconds for time to restart...")
                case Failure(e) =>
                  logger.error("[advance] waiting for MATSim to start the next iteration, failed:")
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
        throw new IllegalStateException(s"advance function should never see matsim simulation in state '$x'")
    }
  }

  override def assignRoutes(simulator: Simulator, responses: List[Response]): SyncIO[Simulator] = SyncIO {

    if (responses.isEmpty) {
      logger.debug(s"[assignRoutes] received 0 route responses - NOOP")
      simulator
    } else {

      logger.debug(s"[assignRoutes] received ${responses.length} route responses")

      val agentsInSimulation: Map[Id[Person], MobsimAgent] = qSim.getAgents.asScala.toMap
      val currentSimTime: SimTime                          = SimTime(self.playPauseSimulationControl.getLocalTime)

      for {
        response    <- responses
        mobsimAgent <- agentsInSimulation.get(Id.createPersonId(response.request.agent))

      } {

        val leg                                   = WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent) // agent is in simulation, so this is safe
        val departureTime                         = MATSimProxy.DepartureTime(leg.getDepartureTime.toInt)
        val agentId: Id[Person]                   = mobsimAgent.getId
        val agentExperiencedRoute: List[Id[Link]] = MATSimProxy.convertToCompleteRoute(leg)
        val routingResultPath: List[Id[Link]] = response.path.map { edgeId =>
          Id.createLinkId(edgeId.value)
        }

        // make sure start and end aren't part of the path, and, add the routingResult to the pathPrefix
        val updatedRoute: List[Id[Link]] =
          MATSimOps.coalescePath(agentExperiencedRoute, routingResultPath)

        logger.debug(s"[assignRoutes] updated route for agent ${agentId.toString} : ${updatedRoute.mkString("->")}")

        // update the mobsim
        val route = RouteUtils.createNetworkRoute(updatedRoute.asJava, qSim.getNetsimNetwork.getNetwork)
        WithinDayAgentUtils.getModifiableCurrentLeg(mobsimAgent).setRoute(route)

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

        self.agentsInSimulationNeedingReplanningHandler.getReplanningCountForAgent(agentId) match {
          case None =>
            throw new IllegalStateException(s"cannot find data on recent (re)-planning for agent $agentId even though that data was just added")
          case Some(numberOfPathAssignments) =>
            logger.debug(s"agent $agentId route #$numberOfPathAssignments assigned at SimTime $currentSimTime")
        }
      }

      // ok! these route requests have been serviced.
//      newRouteRequests = None

      logger.debug(s"[assignRoutes] modified agents in MATSim based on route responses")
      simulator
    }
  }

  override def getState(simulator: Simulator): SyncIO[Either[String, SimulatorState]] = SyncIO {

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
      case SimulatorState.Running if !t.isAlive =>
        matsimState = SimulatorState.Finished
        Left(
          MATSimSimulation.IsDoneFailure.TimeoutFailure(s"MATSim may have had a fatal error").toString
        )

      case other => Right(other)
    }
  }

  override def getUpdatedEdges(simulator: Simulator): SyncIO[List[(EdgeId, Flow)]] = SyncIO {
    val result = for {
      (linkId, count) <- roadNetworkDeltaHandler.getDeltas.toList
    } yield (EdgeId(linkId.toString), Flow(count))
    val numCounts = if (result.isEmpty) 0 else result.map { _._2.value }.sum
    logger.debug(s"[getUpdatedEdges] has ${result.length} entries with aggregate (sum) flow effect of $numCounts")
    result
  }

  /**
    * constructs [[Request]] objects for each active agent
    *
    * @return a list of requests
    */
  override def getActiveRequests(simulator: Simulator): SyncIO[List[Request]] = SyncIO {
    if (matsimState != SimulatorState.Running) {
      List.empty
    } else {

      var i: Int = 0
      while (routingRequestsUpdatedToTimeStep < SimTime(playPauseSimulationControl.getLocalTime)) {
        Try {
          Thread.sleep(100)
        } match {
          case Success(()) =>
            i = i + 1
            if (i % 10 == 0) {
              logger.debug(s"[getActiveRequests] waited ${i / 10} seconds for routing requests to be generated...")
            }
          case Failure(e) =>
            logger.error("waiting for routing requests to be updated by child thread, failed:")
            throw e
        }
      }

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

  override def getCurrentSimTime(simulator: Simulator): SyncIO[SimTime] = SyncIO {
    val currentTime = SimTime(playPauseSimulationControl.getLocalTime)
    logger.debug(s"[getCurrentSimTime] $currentTime")
    currentTime
  }
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

  val RequestClassAttributeLabel: String = "requestclass"

  def getRequestClass(person: Person): Option[RequestClass] = {
    val requestClassString: String =
      person.getAttributes.getAttribute(RequestClassAttributeLabel).asInstanceOf[String]
    RequestClass(requestClassString)
  }

  def pathHasAtLeastTwoLinks(path: List[Id[Link]]): Boolean = {
    path.nonEmpty && path.tail.drop(1).nonEmpty
  }

  final case class DepartureTime(value: Int) extends AnyVal

  private final case class ReplanningData(id: Id[Person], current: Id[Link], destination: Id[Link], route: List[Id[Link]])

  private[MATSimProxy] final case class ReasonableStartPointFoldAccumulator(
    remainingSlack: TravelTimeSeconds,
    estimatedRemainingTravelTime: TravelTimeSeconds = TravelTimeSeconds.Zero,
    startPoint: Option[Id[Link]] = None,
    pathPrefix: List[Id[Link]] = List.empty
  ) {
    def startPointFound: Boolean = startPoint.nonEmpty
    def startPointNotFound: Boolean = startPoint.isEmpty
    def clearedReplanningLeadTime: Boolean = remainingSlack <= TravelTimeSeconds.Zero
  }

  def convertToCompleteRoute(leg: Leg): List[Id[Link]] = {
    leg.getRoute.getStartLinkId +:
      leg.getRoute.asInstanceOf[NetworkRoute].getLinkIds.asScala.toList :+
      leg.getRoute.getEndLinkId
  }

  def assignCompleteRouteToLeg(links: List[Id[Link]], leg: Leg): Unit = {
    val startEdge              = links.head
    val endEdge                = links.last
    val middle: List[Id[Link]] = links.drop(1).dropRight(1)
    leg.getRoute
      .asInstanceOf[NetworkRoute]
      .setLinkIds(startEdge, middle.asJava, endEdge)
  }

  def selectRequestOriginLink(
    previousPathFromCurrentOrigin: List[Id[Link]],
    currentLinkId: Id[Link],
    destinationLinkId: Id[Link],
    qSim: QSim,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds
  ): Option[EdgeId] = {

    previousPathFromCurrentOrigin.dropWhile { _ != currentLinkId } match {
      case Nil =>
        // didn't find current edge in agent's route, which is an error; drop this route request.
        None
      case previousPath =>
        // find a reasonable start point, which may be some point in the future,
        // but if there isn't enough "slack" remaining in their route for replan,
        // then don't touch it.
        val search: ReasonableStartPointFoldAccumulator =
          previousPath
            .map { l =>
              val link: Link = qSim.getNetsimNetwork
                .getNetsimLink(Id.createLinkId(l.toString))
                .getLink
              (l, Meters.toTravelTime(Meters(link.getLength), MetersPerSecond(link.getFreespeed)))
            }
            .foldLeft(MATSimProxy.ReasonableStartPointFoldAccumulator(reasonableReplanningLeadTime)) { (acc, tup) =>
//              acc.startPoint match {
//                case Some(_) =>
//                  // already found - skip
//                  acc
//                case None =>
              val (linkId, linkTravelTime)           = tup
              val nextRemainingSlack: TravelTimeSeconds = acc.remainingSlack - linkTravelTime
              if (acc.clearedReplanningLeadTime && acc.startPointNotFound) {
                // reasonable start point has been found. store it, and begin storing est. remaining travel time.
                acc.copy(
                  remainingSlack = nextRemainingSlack,
                  estimatedRemainingTravelTime = linkTravelTime,
                  startPoint = Some {
                    linkId
                  }
                )
              } else if (acc.clearedReplanningLeadTime && acc.startPointFound) {
                // reasonable start point was found before. accumulate estimated remaining trip costs
                val nextEstRemainingTravelTime = acc.estimatedRemainingTravelTime + linkTravelTime
                acc.copy(
                  estimatedRemainingTravelTime = nextEstRemainingTravelTime
                )
              } else {
                // searching for reasonable start point. update
                acc.copy(
                  remainingSlack = nextRemainingSlack,
                  pathPrefix = acc.pathPrefix :+ linkId
                )
              }
//              }
            }
        for {
          possibleStartPoint <- search.startPoint
          if minimumRemainingRouteTimeForReplanning < search.estimatedRemainingTravelTime
        } yield EdgeId(possibleStartPoint.toString)
    }
  }

  def resolvePayloads(olderPayload: RouteRequests, newerPayload: RouteRequests): RouteRequests = {
    // count all flow delta data
    val updatedRoadNetworkDeltas: Map[EdgeId, Int] = {
      for {
        edgeId <- olderPayload.networkDeltas.keySet.union(newerPayload.networkDeltas.keySet)
        aFlows = olderPayload.networkDeltas.getOrElse(edgeId, 0)
        bFlows = newerPayload.networkDeltas.getOrElse(edgeId, 0)
      } yield {
        edgeId -> (aFlows + bFlows)
      }
    }.toMap
    RouteRequests(
      newerPayload.timeOfDay,
      (olderPayload.requests ++ newerPayload.requests).distinct,
      updatedRoadNetworkDeltas
    )
  }

  def batchEndTimesInRange(startTime: SimTime, endTime: SimTime, batchWindow: SimTime): List[SimTime] = {
    val it = Iterator.iterate(startTime.value.toInt){_ + 1}.dropWhile{_ % batchWindow.value.toInt != 0}
    if (it.isEmpty) List.empty
    else {
      {
        for {
          i <- it.next to endTime.value.toInt by batchWindow.value.toInt
        } yield SimTime(i)
      }.toList
    }
  }
}
