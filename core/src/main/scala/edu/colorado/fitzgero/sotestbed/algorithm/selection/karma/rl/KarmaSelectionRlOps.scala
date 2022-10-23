package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness._
import cats.implicits._
import cats.effect._
import java.nio.file.{Path => JavaNioPath}
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import com.typesafe.scalalogging.LazyLogging
import java.io.PrintWriter
import scala.util.Try
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import scala.util.Failure
import scala.util.Success
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.Empty
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.GetActionResponse
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse.StartEpisodeResponse
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentRealAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentRealAction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Bid
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

object KarmaSelectionRlOps extends LazyLogging {

  def startMultiAgentEpisode(
    client: RayRLlibClient,
    episodeId: Option[EpisodeId]
  ): IO[PolicyClientResponse] = {
    val startReq = PolicyClientRequest.StartEpisodeRequest(
      episodeId,
      client.trainingEnabled
    )
    client.sendOne(startReq)
  }

  def startSingleAgentEpisodeForRequests(
    client: RayRLlibClient,
    episodePrefix: String,
    alts: Map[Request, List[Path]],
    agentsWithEpisodes: scala.collection.mutable.Set[String]
  ): IO[List[Request]] = {
    val newReqs = alts.keys.toList.filterNot { r => agentsWithEpisodes.contains(r.agent) }
    val requests = newReqs.toList.map { req =>
      val episodeId = Some(EpisodeId(req.agent, episodePrefix))
      PolicyClientRequest.StartEpisodeRequest(episodeId, client.trainingEnabled)
    }

    // update our record of any active episodes
    client.sendMany(requests, failOnServerError = true).map { _ => newReqs }
  }

  def extractAction(res: PolicyClientResponse): IO[Action] = {
    res match {
      case x: GetActionResponse => IO.pure(x.action)
      case other                => IO.raiseError(new Error(s"expected GetActionResponse, found $other"))
    }
  }

  def getSingleAgentBids(
    client: RayRLlibClient,
    structure: DriverPolicyStructure.SingleAgentPolicy,
    episodePrefix: String,
    alts: Map[Request, List[Path]],
    signal: NetworkPolicySignal,
    bank: Map[String, Karma],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    activeAgentHistory: ActiveAgentHistory,
    logFn: Option[(List[PolicyClientRequest], List[PolicyClientResponse]) => IO[Unit]] = None
  ): IO[List[Bid]] = {
    import PolicyClientRequest._
    import Action._
    import Observation._
    val requests = alts.keys.toList
    val actionReqs: IO[List[PolicyClientRequest]] = alts.toList.traverse {
      case (req, paths) =>
        structure
          .encodeObservation(roadNetwork, activeAgentHistory, bank, req, paths, alts, signal)
          .map { obs =>
            val episodeId = EpisodeId(req.agent, episodePrefix)
            GetActionRequest(episodeId, SingleAgentObservation(obs))
          }
    }
    for {
      reqs      <- actionReqs
      responses <- client.sendMany(reqs, failOnServerError = true)
      _         <- logFn.map { f => f(reqs, responses) }.getOrElse(IO.unit)
      actions   <- responses.traverse(extractAction)
      bidValues <- actions.traverse { structure.decodeSingleAgentActionAsBid }
      reqsWithBids = requests.zip(bidValues)
      bidTrunks <- reqsWithBids.traverse {
        case (req, bidVal) =>
          IO.fromEither(bank.limitBidByBalance(bidVal, req.agent))
            .map { case truncBid => Bid(req, truncBid) }
      }
    } yield bidTrunks
  }

  def getMultiAgentBids(
    client: RayRLlibClient,
    structure: DriverPolicyStructure.MultiAgentPolicy,
    episodeId: EpisodeId,
    alts: Map[Request, List[Path]],
    signal: NetworkPolicySignal,
    bank: Map[String, Karma],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    activeAgentHistory: ActiveAgentHistory,
    logFn: Option[(PolicyClientRequest, PolicyClientResponse) => IO[Unit]] = None
  ): IO[List[Bid]] = {
    import PolicyClientRequest._
    import Action._
    import Observation._
    val requests = alts.keys.toList
    val agentObsResult = alts.toList.traverse {
      case (req, paths) =>
        structure
          .encodeObservation(roadNetwork, activeAgentHistory, bank, req, paths, alts, signal)
          .map { obs => (AgentId(req.agent), obs) }
    }
    for {
      agentObs <- agentObsResult
      mao = MultiAgentObservation(agentObs.toMap)
      gar = GetActionRequest(episodeId, mao)
      response  <- client.sendOne(gar)
      _         <- logFn.map { f => f(gar, response) }.getOrElse(IO.unit)
      action    <- extractAction(response)
      bidValues <- structure.decodeMultiAgentActionAsBid(action)
      reqsWBids <- requests.traverse { r =>
        IO.fromOption(bidValues.get(r.agent).map { k => (r, k) }) {
          new Error(s"action request $r not found in RL server response")
        }
      }
      bidTrunks <- reqsWBids.traverse {
        case (req, bidVal) =>
          IO.fromEither(bank.limitBidByBalance(bidVal, req.agent))
            .map { case truncBid => Bid(req, truncBid) }
      }
    } yield bidTrunks
  }

  def endMultiAgentEpisode(
    episodeId: EpisodeId,
    multiAgentStructure: DriverPolicyStructure.MultiAgentPolicy,
    client: RayRLlibClient,
    networkPolicyConfig: NetworkPolicyConfig,
    experimentDirectory: JavaNioPath,
    allocationTransform: AllocationTransform,
    allocationMetric: AllocationMetric,
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    agentsWithEpisodes: Set[String],
    finalBank: Map[String, Karma],
    logFn: Option[(PolicyClientRequest, PolicyClientResponse) => IO[Unit]] = None
  ) = {
    import PolicyClientRequest._
    import Reward._
    import Observation._
    val finalData = allocationMetric.collectFinalRewardsAndObservations(
      experimentDirectory = experimentDirectory,
      agentsWithEpisodes = agentsWithEpisodes,
      allocationTransform = allocationTransform,
      driverPolicySpace = multiAgentStructure.space,
      networkPolicyConfig = networkPolicyConfig,
      roadNetwork = roadNetwork,
      finalBank = finalBank
    )
    // val finalData = collectFinalRewardsAndObservations(
    //   experimentDirectory = experimentDirectory,
    //   agentsWithEpisodes = agentsWithEpisodes,
    //   allocationTransform = allocationTransform,
    //   driverPolicySpace = multiAgentStructure.space,
    //   networkPolicyConfig = networkPolicyConfig,
    //   finalBank = finalBank
    // )
    for {
      data <- finalData
      (rewards, observations) = data
      rewardValues            = rewards.map { case (a, r) => AgentId(a) -> r }
      mar                     = MultiAgentReward(rewardValues.toMap)
      obsValues               = observations.map { case (a, o) => AgentId(a) -> o }
      mao                     = MultiAgentObservation(obsValues.toMap)
      logReturnsReq           = LogReturnsRequest(episodeId, mar)
      endEpisodeReq           = EndEpisodeRequest(episodeId, mao)
      _ <- client.sendOne(logReturnsReq, failOnServerError = false)
      // needs to happen AFTER logging final reward for all agents
      _ <- client.sendOne(endEpisodeReq, failOnServerError = false)
      _ <- logFn.map { f => f(logReturnsReq, PolicyClientResponse.Empty) }.getOrElse(IO.unit)
      _ <- logFn.map { f => f(endEpisodeReq, PolicyClientResponse.Empty) }.getOrElse(IO.unit)
      _ <- logFinalRewards(rewards, observations, experimentDirectory)
    } yield logger.info(s"finished multi-agent RL episode $episodeId")

  }

  def endSingleAgentEpisodes(
    singleAgentStructure: DriverPolicyStructure.SingleAgentPolicy,
    client: RayRLlibClient,
    networkPolicyConfig: NetworkPolicyConfig,
    experimentDirectory: JavaNioPath,
    allocationTransform: AllocationTransform,
    allocationMetric: AllocationMetric,
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    agentsWithEpisodes: Set[String],
    finalBank: Map[String, Karma],
    episodePrefix: String,
    logFn: Option[(List[PolicyClientRequest], List[PolicyClientResponse]) => IO[Unit]] = None
  ) = {
    import PolicyClientRequest._
    import Reward._
    import Observation._

    // trip log contains the initial travel time estimate along with the final travel time, but not all
    // agents in the trip log can be guaranteed to have RL episodes
    // see [[]]
    // here, we filter the trip log to the agents with known RL episodes before sending requests to log + close
    // those episodes. we also log all of the final rewards and observations to a file.
    val result: IO[Unit] = for {
      data <- allocationMetric.collectFinalRewardsAndObservations(
        experimentDirectory = experimentDirectory,
        agentsWithEpisodes = agentsWithEpisodes,
        allocationTransform = allocationTransform,
        driverPolicySpace = singleAgentStructure.space,
        networkPolicyConfig = networkPolicyConfig,
        roadNetwork = roadNetwork,
        finalBank = finalBank
      )
      (rewardValues, obsValues) = data
      rewards                   = rewardValues.map { case (a, r) => a -> SingleAgentReward(r) }
      observations              = obsValues.map { case (a, o) => a -> SingleAgentObservation(o) }
      logRewardsMsgs            = rewards.map { case (a, r) => LogReturnsRequest(EpisodeId(a, episodePrefix), r) }
      endEpisodeMsgs            = observations.map { case (a, o) => EndEpisodeRequest(EpisodeId(a, episodePrefix), o) }
      // the RL server can die here if it meets it's stopping condition on number of episodes observed
      // but we don't want that to blow up our remaining cleanup after this point
      logRewResponses <- client.sendMany(logRewardsMsgs, failOnServerError = false)
      // needs to happen AFTER logging final reward for all agents
      endEpRepsonses <- client.sendMany(endEpisodeMsgs, failOnServerError = false)
      _              <- logFn.map { f => f(logRewardsMsgs, logRewResponses) }.getOrElse(IO.unit)
      _              <- logFn.map { f => f(endEpisodeMsgs, endEpRepsonses) }.getOrElse(IO.unit)
      _              <- logFinalRewards(rewardValues, obsValues, experimentDirectory)
    } yield logger.info(s"finished all single agent RL episodes")

    result
  }

  // def collectFinalRewardsAndObservations(
  //   experimentDirectory: JavaNioPath,
  //   agentsWithEpisodes: Set[String],
  //   allocationTransform: AllocationTransform,
  //   driverPolicySpace: DriverPolicySpace,
  //   networkPolicyConfig: NetworkPolicyConfig,
  //   finalBank: Map[String, Karma]
  // ): IO[(List[(String, Double)], List[(String, List[Double])])] = {
  //   for {
  //     tripLogs <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
  //     tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
  //     rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByTripComparison(tripsWithEpisodes, allocationTransform)
  //     observations <- RLDriverPolicyEpisodeOps.finalObservations(
  //       tripsWithEpisodes,
  //       driverPolicySpace,
  //       networkPolicyConfig,
  //       finalBank
  //     )
  //   } yield (rewards, observations)
  // }

  // def collectFinalRewardsAndObservationsAuctionDelayMetric(
  //   experimentDirectory: JavaNioPath,
  //   agentsWithEpisodes: Set[String],
  //   allocationTransform: AllocationTransform,
  //   driverPolicySpace: DriverPolicySpace,
  //   networkPolicyConfig: NetworkPolicyConfig,
  //   finalBank: Map[String, Karma]
  // ): IO[(List[(String, Double)], List[(String, List[Double])])] = {
  //   for {
  //     tripLogs  <- RLDriverPolicyEpisodeOps.getTripLog(experimentDirectory)
  //     karmaLogs <- RLDriverPolicyEpisodeOps.getKarmaLog(experimentDirectory)
  //     tripsWithEpisodes = tripLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
  //     karmaWithEpisodes = karmaLogs.filter(row => agentsWithEpisodes.contains(row.agentId))
  //     rewards <- RLDriverPolicyEpisodeOps.endOfEpisodeRewardByAuctionDelay(karmaWithEpisodes, tripsWithEpisodes)
  //     observations <- RLDriverPolicyEpisodeOps.finalObservations(
  //       tripsWithEpisodes,
  //       driverPolicySpace,
  //       networkPolicyConfig,
  //       finalBank
  //     )
  //   } yield (rewards, observations)
  // }

  def logFinalRewards(
    rewards: List[(String, Double)],
    observations: List[(String, List[Double])],
    experimentDirectory: JavaNioPath
  ): IO[Unit] = {
    IO.fromTry {
      Try {
        val pw = new PrintWriter(f"$experimentDirectory/final_rewards.csv")
        pw.write(s"agentId,reward,observation" + "\n")
        val rewardsLookup = rewards.toMap
        for {
          (a, o) <- observations
          r      <- rewardsLookup.get(a)
        } {
          pw.write(f"""$a,$r,"${o.mkString("[", ",", "]")}"""" + "\n")
        }
        pw.close()
      }
    }
  }
}
