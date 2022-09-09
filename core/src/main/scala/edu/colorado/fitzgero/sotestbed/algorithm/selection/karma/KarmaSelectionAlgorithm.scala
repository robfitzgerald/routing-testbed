package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.PrintWriter

import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, TrueShortestSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm.karmaLogHeader
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
import scala.reflect.macros.NonemptyAttachments
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.StartEpisodeResponse

import cats.effect.unsafe.implicits.global

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
  allocationTransform: AllocationTransform
) extends SelectionAlgorithm
    with LazyLogging {

  import KarmaSelectionAlgorithm._

  val selectionLogFile: java.nio.file.Path = experimentDirectory.resolve(KarmaLogFilename)
  val networkLogFile: java.nio.file.Path   = experimentDirectory.resolve(KarmaNetworkLogFilename)
  val selectionPw: PrintWriter             = new PrintWriter(selectionLogFile.toFile)
  val networkPw: PrintWriter               = new PrintWriter(networkLogFile.toFile)

  // create a random prefix for each run of the simulator so that the same agentId may
  // be used across concurrent, overlapping simulations generating training data
  // only used with SingleAgentPolicies
  val episodePrefix: String = java.util.UUID.randomUUID.toString

  // whenever we see an agent start an RL episode, we store that information here
  // only used with SingleAgentPolicies
  val agentsWithEpisodes: mutable.Set[String] = mutable.Set.empty

  // in the case of a MultiAgentPolicy, we create exactly one episode per experiment
  // and start the episode now
  val multiAgentEpisodeId: Option[EpisodeId] = driverPolicy match {
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

  val networkLogHeader: String = List(
    "batchId",
    networkPolicy.logHeader,
    NetworkPolicySignal.getLogHeader(networkPolicy)
  ).mkString(",")

  selectionPw.write(karmaLogHeader + "\n")
  networkPw.write(networkLogHeader + "\n")

  val gen: NetworkPolicySignalGenerator = networkPolicy.buildGenerator

  /**
    * this close method is used as it is called in RoutingExperiment2's .close() method
    */
  def close(finalBank: Map[String, Karma]): IO[Unit] = {
    logger.info(s"closing $KarmaLogFilename")
    selectionPw.close()
    logger.info(s"closing $KarmaNetworkLogFilename")
    networkPw.close()
    driverPolicy match {
      case RLBasedDriverPolicy(structure, client) =>
        logger.info(s"sending final messages to RL server")
        structure match {
          case multiAgentPolicy: MultiAgentPolicy =>
            val epId = IO.fromOption(this.multiAgentEpisodeId)(new Error("missing EpisodeId for multiagent policy"))
            for {
              episodeId <- epId
              _ <- KarmaSelectionRlOps.endMultiAgentEpisode(
                episodeId,
                multiAgentPolicy,
                client,
                networkPolicy,
                experimentDirectory,
                allocationTransform,
                agentsWithEpisodes.toSet,
                finalBank
              )
            } yield ()

          case singleAgentPolicy: SingleAgentPolicy =>
            KarmaSelectionRlOps.endSingleAgentEpisodes(
              singleAgentPolicy,
              client,
              networkPolicy,
              experimentDirectory,
              allocationTransform,
              agentsWithEpisodes.toSet,
              finalBank,
              episodePrefix
            )
        }

      case _ => IO.unit
    }
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
    networkObservations: Map[String, CongestionObservationResult],
    networkPolicySignals: Map[String, NetworkPolicySignal],
    selectionLog: PrintWriter,
    networkLog: PrintWriter
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
                client.send(requests)
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

              // val agentDoneResult: IO[Unit] = structure match {
              //   case sap: SingleAgentPolicy => ()
              //   case map: MultiAgentPolicy =>
              //     val agents
              // }

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

        // run the driver policy and network policy, and use the result to select
        // a path for each driver agent
        // get the costs associated with the trips
        val result = for {
          _      <- updateRlClientServerResult
          signal <- IO.fromEither(networkPolicySignals.getOrError(batchId))
          bids <- driverPolicy
            .applyDriverPolicy(
              signal,
              alts,
              bank,
              activeAgentHistory,
              roadNetwork,
              costFunction,
              episodePrefix,
              multiAgentEpisodeId
            )
          selections = signal.assign(bids, alts)
          paths      = selections.map { case (_, _, path) => path }
          routesUo   = alts.values.flatMap(_.headOption).toList
          costsUo     <- collabCostFn(routesUo)
          costsSo     <- collabCostFn(paths)
          updatedBank <- IO.fromEither(auctionPolicy.resolveAuction(selections, bank, bankConfig.max))
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

          // log info about the karma selection process
          val bidLookup = bids.map { b => b.request.agent -> b }.toMap
          val loggingOrError = responses.traverse { response =>
            val agent = response.request.agent
            for {
              bid          <- bidLookup.getOrError(agent)
              startBalance <- bank.getOrError(agent)
              endBalance   <- updatedBank.getOrError(agent)
            } yield {
              val selectionRow = KarmaSelectionLogRow(
                batchId = batchId,
                agentId = agent,
                startBalance = startBalance,
                endBalance = endBalance,
                bidValue = bid.value,
                route = response.pathIndex
              )

              selectionRow
            }
          }

          val innerResult = loggingOrError.map { logDataList =>
            // log karma data for each agent
            logDataList.foreach { selectionRow => selectionLog.write(selectionRow.toLogRow + "\n") }

            // log the network policy for this batch
            val networkRow = List(batchId, networkPolicy.getLogData, signal.getLogData).mkString(",")
            networkLog.write(networkRow + "\n")

            selectionLog.flush()
            networkLog.flush()

            selectionAlgorithmResult
          }

          IO.fromEither(innerResult)
        }

        result.flatten
      }
    }
  }
}

object KarmaSelectionAlgorithm {

  val karmaLogHeader: String  = "batchId,agentId,startKarma,endKarma,bidValue,selectedRoute"
  val KarmaLogFilename        = "karma_log.csv"
  val KarmaNetworkLogFilename = "karma_network_log.csv"

  final case class KarmaBatchData(batchId: String, obs: CongestionObservationResult, signal: NetworkPolicySignal)

  case class KarmaSelectionLogRow(
    batchId: String,
    agentId: String,
    startBalance: Karma,
    endBalance: Karma,
    bidValue: Karma,
    route: Int
  ) {

    def toLogRow: String =
      s"$batchId,$agentId,$startBalance,$endBalance,$bidValue,$route"
  }
}
