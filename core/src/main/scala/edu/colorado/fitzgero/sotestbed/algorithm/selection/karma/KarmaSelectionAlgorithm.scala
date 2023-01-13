package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.PrintWriter

import cats.effect.IO
import cats.implicits._

import io.circe.syntax._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, TrueShortestSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.config.{BankConfig, FreeFlowCostFunctionConfig}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.DriverPolicy._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness._
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure.SingleAgentPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure.MultiAgentPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.KarmaSelectionRlOps
import scala.collection.mutable
import scala.util.Random
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.StartEpisodeResponse

import cats.effect.unsafe.implicits.global
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ExternalRLServer
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.numeric.TravelTimeSeconds
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.UserOptimal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.CongestionThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.CongestionWeightedSampling
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ScaledProportionalThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.RayRLlibClient

case class KarmaSelectionAlgorithm(
  driverPolicy: DriverPolicy,
  networkPolicy: NetworkPolicyConfig,
  auctionPolicy: AuctionPolicy,
  congestionObservation: CongestionObservationType,
  bankConfig: BankConfig,
  freeFlowCostFunction: FreeFlowCostFunctionConfig,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  seed: Option[Long],
  experimentDirectory: java.nio.file.Path,
  allocationMetric: AllocationMetric,
  allocationTransform: AllocationTransform,
  useStepWiseRewards: Boolean
) extends SelectionAlgorithm
    with LazyLogging {

  import KarmaSelectionAlgorithm._

  val selectionLogPath: java.nio.file.Path     = experimentDirectory.resolve(KarmaSelectionLogRow.KarmaLogFilename)
  val networkLogPath: java.nio.file.Path       = experimentDirectory.resolve(KarmaNetworkLogFilename)
  val driverClientLogPath: java.nio.file.Path  = experimentDirectory.resolve(DriverClientLogFilename)
  val networkClientLogPath: java.nio.file.Path = experimentDirectory.resolve(NetworkClientLogFilename)

  val selectionPw: PrintWriter     = new PrintWriter(selectionLogPath.toFile)
  val networkPw: PrintWriter       = new PrintWriter(networkLogPath.toFile)
  val driverClientPw: PrintWriter  = new PrintWriter(driverClientLogPath.toFile)
  val networkClientPw: PrintWriter = new PrintWriter(networkClientLogPath.toFile)
  // todo: intercept all client communications and write to json newline-formatted file
  //  - maybe RayRLlibClient.send takes an optional callback for logging?

  // create a random prefix for each run of the simulator so that the same agentId may
  // be used across concurrent, overlapping simulations generating training data
  // only used with SingleAgentPolicies
  val episodePrefix: String = java.util.UUID.randomUUID.toString

  // whenever we see an agent start an RL episode, we store that information here
  // only used with SingleAgentPolicies
  val agentsWithEpisodes: mutable.Set[String] = mutable.Set.empty

  // in the case of a MultiAgentPolicy, we create exactly one episode per experiment
  // and start the episode now
  val multiAgentDriverPolicyEpisodeId: Option[EpisodeId] = driverPolicy match {
    case RLBasedDriverPolicy(structure, client) =>
      structure match {
        case _: MultiAgentPolicy =>
          val episodeId = EpisodeId()
          KarmaSelectionRlOps.startMultiAgentEpisode(client, Some(episodeId)).unsafeRunSync()
          Some(episodeId)
        case _: SingleAgentPolicy => None
      }
    case _ => None
  }

  val multiAgentNetworkPolicyEpisodeId: Option[EpisodeId] = networkPolicy match {
    case ExternalRLServer(underlying, structure, client) =>
      // only multi-agent
      val episodeId = EpisodeId()
      KarmaSelectionRlOps.startMultiAgentEpisode(client, Some(episodeId)).unsafeRunSync()
      Some(episodeId)
    case _ => None
  }

  val networkLogHeader: String = List(
    "batchId",
    networkPolicy.logHeader,
    NetworkPolicySignal.getLogHeader(networkPolicy)
  ).mkString(",")

  selectionPw.write(KarmaSelectionLogRow.KarmaLogHeader + "\n")
  networkPw.write(networkLogHeader + "\n")

  val gen: NetworkPolicySignalGenerator = networkPolicy.buildGenerator

  // RL-based network policy needs to know what "agents" submitted GET_ACTION
  // requests last time step in order to reward them in the future
  private var previousBatch: Map[String, List[EdgeId]] = Map.empty

  def updatePreviousBatch(batch: Map[String, List[EdgeId]]): Unit = {
    previousBatch = batch
  }
  def getPreviousBatchIds(): List[String]           = previousBatch.keys.toList
  def getPreviousBatch(): Map[String, List[EdgeId]] = previousBatch

  /**
    * this close method is used as it is called in RoutingExperiment2's .close() method
    */
  def close(
    finalBank: Map[String, Karma],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[Unit] = {
    logger.info(s"closing ${KarmaSelectionLogRow.KarmaLogFilename}")
    selectionPw.close()
    logger.info(s"closing $KarmaNetworkLogFilename")
    networkPw.close()
    driverClientPw.close()
    networkClientPw.close()
    val networkPolicyResult = networkPolicy match {
      case ExternalRLServer(underlying, structure, client) =>
        val episodeId =
          IO.fromOption(this.multiAgentNetworkPolicyEpisodeId)(new Error("missing EpisodeId for multiagent policy"))
        for {
          epId <- episodeId
          lastBatches = this.getPreviousBatch()
          space <- IO.fromOption(underlying.space)(new Error("network policy has no 'space'"))
          rew   <- space.encodeReward(roadNetwork, lastBatches)
          mao   <- space.encodeObservation(roadNetwork, lastBatches)
          req1: PolicyClientRequest = PolicyClientRequest.LogReturnsRequest(epId, rew)
          req2: PolicyClientRequest = PolicyClientRequest.EndEpisodeRequest(epId, mao)
          res1 <- client.sendOne(req1)
          res2 <- client.sendOne(req2)
          _ = networkClientPw.write(req1.asJson.noSpaces.toString + "\n")
          _ = networkClientPw.write(req2.asJson.noSpaces.toString + "\n")
        } yield ()
      case _ => IO.unit
    }
    val driverPolicyResult = driverPolicy match {
      case RLBasedDriverPolicy(structure, client) =>
        logger.info(s"sending final messages to RL server")
        structure match {
          case multiAgentPolicy: MultiAgentPolicy =>
            val epId =
              IO.fromOption(this.multiAgentDriverPolicyEpisodeId)(new Error("missing EpisodeId for multiagent policy"))
            for {
              episodeId <- epId
              _ <- KarmaSelectionRlOps.endMultiAgentEpisode(
                episodeId,
                multiAgentPolicy,
                client,
                networkPolicy,
                experimentDirectory,
                allocationTransform,
                allocationMetric,
                roadNetwork,
                agentsWithEpisodes.toSet,
                finalBank,
                Some(RayRLlibClient.standardSendOneLogFn(driverClientPw))
              )
            } yield ()

          case singleAgentPolicy: SingleAgentPolicy =>
            KarmaSelectionRlOps.endSingleAgentEpisodes(
              singleAgentPolicy,
              client,
              networkPolicy,
              experimentDirectory,
              allocationTransform,
              allocationMetric,
              roadNetwork,
              agentsWithEpisodes.toSet,
              finalBank,
              episodePrefix,
              Some(RayRLlibClient.standardSendManyLogFn(driverClientPw))
            )
        }

      case _ => IO.unit
    }
    for {
      _ <- driverPolicyResult
      _ <- networkPolicyResult
    } yield ()
  }

  /**
    * sorry...
    * @deprecated call build first, then use the resulting class to solve selection
    */
  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    ignoreCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] =
    IO.raiseError(new Error("cannot call selectRoutes before calling KarmaSelectionAlgorithm.build()"))

  /**
    * builds a selection algorithm for karma problems with the current simulation state
    * @param activeAgentHistory current active agent history
    * @param networkPolicySignals signal from the NetworkPolicy
    * @return a selection algortihm for karma-based problems
    */
  def build(
    activeAgentHistory: ActiveAgentHistory,
    networkPolicySignals: Map[String, NetworkPolicySignal],
    selectionLog: PrintWriter,
    networkLog: PrintWriter,
    clientLog: PrintWriter
  ): SelectionAlgorithm = new SelectionAlgorithm {

    import KarmaSelectionAlgorithm._

    def selectRoutes(
      batchId: String,
      alts: Map[Request, List[Path]],
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      bank: Map[String, Karma],
      pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      ignoreMeToo: EdgeBPR => Flow => Cost
    ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

      if (alts.isEmpty) IO {
        SelectionAlgorithm.SelectionAlgorithmResult(updatedBank = bank)
      }
      else if (alts.size == 1) {

        TrueShortestSelectionAlgorithm().selectRoutes(
          "user-optimal",
          alts,
          roadNetwork,
          bank,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
      } else if (!networkPolicySignals.isDefinedAt(batchId)) {
        logger.error(
          f"""batch $batchId not present in network signals! 
             |found ${networkPolicySignals.size} batchIds present""".stripMargin
        )
        val result = SelectionAlgorithm.SelectionAlgorithmResult(updatedBank = bank)
        IO.pure(result)
      } else {

        // we may need to start some RL episodes. RLlib has undefined behavior when we
        // start an episode for an agent but close it before making any observations.
        // by dealing with starting episodes here, we are only starting episodes for
        // agents that will have at least one observation, made below. this only
        // applies for SingleAgentPolicy problems.
        val updateRlClientServerResult = driverPolicy match {
          case RLBasedDriverPolicy(structure, client) =>
            val newReqs = alts.keys.toList.filterNot { r => agentsWithEpisodes.contains(r.agent) }

            val startEpResult: IO[List[PolicyClientResponse]] = structure match {
              case map: MultiAgentPolicy =>
                // nothing to do, though we still track newly-observed agents below
                IO.pure(List.empty)
              case sap: SingleAgentPolicy =>
                val requests = newReqs.toList.map { req =>
                  val episodeId = Some(EpisodeId(req.agent, episodePrefix))
                  PolicyClientRequest.StartEpisodeRequest(episodeId, client.trainingEnabled)
                }
                client.sendMany(requests, failOnServerError = true)
            }

            startEpResult.map { responses =>
              // update local information about active RL agents (Single or MultiAgent)
              val sizeBefore = agentsWithEpisodes.size
              agentsWithEpisodes.addAll(newReqs.map { _.agent })

              // log newly-activated SingleAgent episodes
              if (responses.nonEmpty) {
                val sendMsg =
                  responses
                    .flatMap {
                      case StartEpisodeResponse(episode_id) => episode_id.value
                      case _                                => ""
                    }
                    .mkString("sent start episode requests for agents ", ",", "")
                logger.info(sendMsg)
              }

              val sizeAfter = agentsWithEpisodes.size
              logger.info(s"now tracking $sizeAfter RL agents up from $sizeBefore")
            }

          case _ => IO.unit
        }

        val costFunction = (e: EdgeBPR) => marginalCostFunction(e)(Flow.Zero)
        val collabCostFn =
          (paths: List[Path]) =>
            SelectionAlgorithm.evaluateCostOfSelection(
              paths,
              roadNetwork,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            )
        val bidFn: NetworkPolicySignal => IO[List[Bid]] = driverPolicy
          .applyDriverPolicy(
            alts,
            bank,
            activeAgentHistory,
            roadNetwork,
            episodePrefix,
            multiAgentDriverPolicyEpisodeId,
            Some(RayRLlibClient.standardSendOneLogFn(driverClientPw))
          )

        // run the driver policy and network policy, and use the result to select
        // a path for each driver agent
        // get the costs associated with the trips
        val result = for {
          _      <- updateRlClientServerResult
          signal <- IO.fromEither(networkPolicySignals.getOrError(batchId))
          bids   <- bidFn(signal)
          selections = signal.assign(bids, alts)
          updatedBank <- IO.fromEither(auctionPolicy.resolveAuction(selections, bank, bankConfig.max))
          paths    = selections.map { case (_, _, path) => path }
          routesUo = alts.values.flatMap(_.headOption).toList
          costsUo <- collabCostFn(routesUo)
          costsSo <- collabCostFn(paths)
        } yield {
          // construct the responses
          val responses = selections.zip(costsSo.agentPathCosts).map {
            case ((bid, index, path), cost) =>
              val edgeList = path.map {
                _.edgeId
              }
              Response(bid.request, index, edgeList, cost)
          }

          // some stuff for logging
          val selfishCost = costsUo.overallCost
          val optimalCost = costsSo.overallCost
          val avgAlts: Double =
            if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
          val travelTimeDiff: Cost     = optimalCost - selfishCost
          val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)
          val (assignedUo, assignedSo) = selections.partition { case (_, idx, _) => idx == 0 }
          logger.info(f"BATCH $batchId")
          logger.info(f"KARMA - NETWORK SIGNAL $signal")
          logger.info(f"BIDS - ${bids.mkString("[", ",", "]")}")
          logger.info(f"ROUTES - ${assignedUo.length} UO | ${assignedSo.length} SO")
          logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: 1")
          logger.info(
            f"COST_EST: BEST $optimalCost, SELFISH $selfishCost, " +
              f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
          )

          // responses and analytics
          val selectionAlgorithmResult = SelectionAlgorithm.SelectionAlgorithmResult(
            selectedRoutes = responses,
            estimatedCost = optimalCost,
            selfishCost = selfishCost,
            travelTimeDiff = travelTimeDiff,
            averageTravelTimeDiff = meanTravelTimeDiff,
            samples = NonNegativeNumber.One,
            updatedBank = updatedBank,
            ratioOfSearchSpaceExplored = 0.0
          )

          // handle logging of stepwise rewards if requested. mvp: assumes we are in a multi-agent environment
          val logReturnsResult =
            if (!useStepWiseRewards || selections.isEmpty) IO.unit
            else
              driverPolicy match {
                case RLBasedDriverPolicy(structure, client) =>
                  val episodeIdResult =
                    IO.fromOption(multiAgentDriverPolicyEpisodeId)(
                      new Error("missing EpisodeId for multiagent policy")
                    )

                  // create a batch-wise allocation for each agent
                  val allocationsResult = alts.toList.zip(selections).traverse {
                    case (((req, _), (_, _, path))) =>
                      allocationMetric.batchWiseAllocation(
                        request = req,
                        selectedPathSpur = path,
                        aah = activeAgentHistory,
                        rn = roadNetwork
                      )
                  }

                  // compute rewards from allocations
                  // log rewards to the RLlib server for this batch
                  // this is done at the same time step but is an estimate of the reward value
                  // due to the action chosen and outcome of the bidding process.
                  for {
                    episodeId   <- episodeIdResult
                    allocations <- allocationsResult
                    rewards <- IO.fromOption(JainFairnessMath.userFairness(allocations))(
                      new Error(s"should not be called on empty batch")
                    )
                    _              = logger.info(f"BATCH-WISE ALLOCATIONS: ${allocations.mkString("[", ", ", "]")}")
                    _              = logger.info(f"BATCH-WISE REWARDS: ${rewards.mkString("[", ", ", "]")}")
                    rewardsByAgent = alts.keys.toList.map(r => AgentId(r.agent)).zip(rewards).toMap
                    req = PolicyClientRequest
                      .LogReturnsRequest(episodeId, Reward.MultiAgentReward(rewardsByAgent))
                    _ <- client.sendOne(req, logFn = Some(RayRLlibClient.standardSendOneLogFn(driverClientPw)))
                  } yield ()
                case _ => IO.unit
              }

          // log info about the karma selection process
          val bidLookup = bids.map { b => b.request.agent -> b }.toMap
          val loggingOrError = logReturnsResult.flatMap { _ =>
            responses.traverse { response =>
              val agent = response.request.agent
              for {
                bid            <- IO.fromEither(bidLookup.getOrError(agent))
                startBalance   <- IO.fromEither(bank.getOrError(agent))
                endBalance     <- IO.fromEither(updatedBank.getOrError(agent))
                currentReqData <- IO.fromEither(activeAgentHistory.getNewestDataOrError(response.request.agent))
                currentTime = currentReqData.timeOfRequest
                selectionLogTimes <- findSelectionLogTimes(response, activeAgentHistory, alts, roadNetwork)
              } yield {
                val selectionRow = KarmaSelectionLogRow(
                  batchId = batchId,
                  agentId = agent,
                  requestTime = currentTime.value,
                  startBalance = startBalance,
                  endBalance = endBalance,
                  bidValue = bid.value,
                  route = response.pathIndex,
                  requestTimeEstimateSeconds = selectionLogTimes.currentEst.value,
                  afterSelectionEstimateSeconds = selectionLogTimes.revisedEst.value
                )

                selectionRow
              }
            }
          }

          val loggingResult = loggingOrError.flatMap { logDataList =>
            IO.fromTry(Try {

              // log karma data for each agent
              logDataList.foreach { selectionRow => selectionLog.write(selectionRow.toLogRow + "\n") }

              // log the network policy for this batch
              val networkRow = List(batchId, networkPolicy.getLogData, signal.getLogData).mkString(",")
              networkLog.write(networkRow + "\n")

              selectionLog.flush()
              networkLog.flush()
              clientLog.flush()

              selectionAlgorithmResult
            })
          }

          loggingResult
        }

        result.flatten
      }
    }
  }
}

object KarmaSelectionAlgorithm {

  val KarmaNetworkLogFilename  = "karma_network_log.csv"
  val DriverClientLogFilename  = "driver_client_log.json"
  val NetworkClientLogFilename = "network_client_log.json"

  final case class KarmaBatchData(batchId: String, obs: CongestionObservationResult, signal: NetworkPolicySignal)

  final case class SelectionLogTimes(currentEst: SimTime, revisedEst: SimTime)

  def findSelectionLogTimes(
    response: Response,
    activeAgentHistory: ActiveAgentHistory,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR]
  ): IO[SelectionLogTimes] = {
    val ttFn: EdgeId => IO[TravelTimeSeconds] = getCurrentTravelTime(roadNetwork)
    for {
      current    <- IO.fromEither(activeAgentHistory.getNewestDataOrError(response.request.agent))
      currentEst <- IO.fromEither(current.overallTravelTimeEstimate)
      paths      <- IO.fromOption(alts.get(response.request))(new Error(s"alts missing request"))
      path       <- IO.fromTry(Try { paths(response.pathIndex) })
      pathTT     <- path.traverse { seg => ttFn(seg.edgeId) }
      revisedEst = pathTT.foldLeft(SimTime.Zero) { (acc, t) => acc + SimTime(t.value) }
    } yield SelectionLogTimes(currentEst, revisedEst)
  }

  def getCurrentTravelTime(roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR])(edgeId: EdgeId): IO[TravelTimeSeconds] = {
    for {
      eaOpt <- roadNetwork.edge(edgeId)
      ea    <- IO.fromOption(eaOpt)(new Error(s"network missing edge $edgeId"))
    } yield ea.attribute.observedTravelTime
  }
}
