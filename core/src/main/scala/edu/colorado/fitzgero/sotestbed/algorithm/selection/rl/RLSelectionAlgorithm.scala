package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import scala.util.Try

import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest.{
  EndEpisodeRequest,
  GetActionRequest,
  LogReturnsRequest,
  StartEpisodeRequest
}
import edu.colorado.fitzgero.sotestbed.rllib.{AgentId, EpisodeId, Observation, PolicyClientOps}

final class RLSelectionAlgorithm(
  val host: String,
  val port: Int,
  val env: Env,
  val episodeId: EpisodeId
) extends SelectionAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  /**
    * selects routes using an RL policy via an external ray.rllib server
    *
    * @param alts
    * @param roadNetwork
    * @param pathToMarginalFlowsFunction
    * @param combineFlowsFunction
    * @param marginalCostFunction
    * @return
    */
  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

    // calculate cost of an assignment based on current road network conditions
    def costFn =
      (paths: List[Path]) =>
        SelectionAlgorithm.evaluateCostOfSelection(
          paths,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )

    val cf = (edge: EdgeBPR) => marginalCostFunction(edge)(Flow.Zero)

    // query the client to get an action
    val result: IO[SelectionAlgorithm.SelectionAlgorithmResult] = for {
      selfish       <- costFn(alts.values.flatMap { _.headOption }.toList)
      observation   <- IO.fromEither(env.encodeObservation(cf)(roadNetwork, alts))
      response      <- PolicyClientOps.send(GetActionRequest(episodeId, observation), host, port)
      action        <- response.getAction
      decodedAction <- IO.fromEither(env.decodeAction(action, alts))
      selectedRoutes = RLSelectionAlgorithm.actionToPaths(decodedAction, alts)
      optimal <- costFn(selectedRoutes.map { case (_, path) => path }.toList)
      optimalCost = optimal.overallCost
      responses <- IO.fromEither(
        RLSelectionAlgorithm.toResponses(decodedAction, selectedRoutes, optimal.agentPathCosts)
      )
      reward <- IO.fromEither(env.encodeReward(selfish, optimal, alts))
      // todo: actually create "infos" and "dones" here
      info = Map.empty[String, String]
      done = Some(Map(AgentId("__all__") -> false))
      _ <- PolicyClientOps.send(LogReturnsRequest(episodeId, reward, info, done), host, port)
    } yield {

      val selfishCost = selfish.overallCost

//      val searchSpaceSize = alts.values.map { l => BigDecimal(l.size) }.product
      val avgAlts: Double =
        if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
      val travelTimeDiff: Cost     = optimalCost - selfishCost
      val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)

      logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: 1")
      logger.info(
        f"COST_EST: BEST $optimalCost, SELFISH $selfishCost, " +
          f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
      )

      val selectionAlgorithmResult = SelectionAlgorithm.SelectionAlgorithmResult(
        selectedRoutes = responses,
        estimatedCost = optimalCost,
        selfishCost = selfishCost,
        travelTimeDiff = travelTimeDiff,
        averageTravelTimeDiff = meanTravelTimeDiff,
        samples = NonNegativeNumber.One,
        ratioOfSearchSpaceExplored = 0.0
      )
      selectionAlgorithmResult
    }

    result
  }

  def reportAgentsAreDone(): IO[Unit] = {
    // create a dummy observation and dummy reward from the grouping
    for {
      observation <- IO.fromEither(env.emptyObservation)
      reward      <- IO.fromEither(env.emptyReward)
      info = Map.empty[String, String]
      done = Some(Map(AgentId("__all__") -> true))
      _ <- PolicyClientOps.send(GetActionRequest(episodeId, observation), host, port)
      _ <- PolicyClientOps.send(LogReturnsRequest(episodeId, reward, info, done), host, port)
    } yield ()
  }

  def close(): IO[Unit] = {
    for {
      obs <- IO.fromEither(env.emptyObservation)
      _   <- PolicyClientOps.send(EndEpisodeRequest(episodeId, obs), host, port)
    } yield ()
  }
}

object RLSelectionAlgorithm {

  def apply(host: String, port: Int, env: Env): IO[RLSelectionAlgorithm] = {
    for {
      res       <- PolicyClientOps.send(StartEpisodeRequest(), host, port)
      episodeId <- res.getEpisodeId
    } yield new RLSelectionAlgorithm(host, port, env, episodeId)
  }

  /**
    * uses the provided action to select paths for agents.
    * if the Request is not mentioned in the action, it is ignored.
    * maintains ordering found in the alts collection
    *
    * @param action multiagent action to apply
    * @param alts requests and their path alternatives
    * @return the path selection for each agent
    */
  def actionToPaths(
    action: Map[AgentId, Int],
    alts: Map[Request, List[Path]]
  ): Map[Request, Path] = {
    //    val lookup = alts.map { case (req, paths) => (AgentId(req.agent), (req, paths)) }
    val result = alts.foldLeft(Map.empty[Request, Path]) {
      case (acc, (request, paths)) =>
        action.get(AgentId(request.agent)) match {
          case None => acc
          case Some(idx) =>
            val selected = Try { paths(idx) }.getOrElse(paths.last)
            acc.updated(request, selected)
        }
    }

    result
  }

  /**
    * combine the requests, action, selected routes, and optimal cost estimates into
    * [[Response]] objects to return to the simulator.
    *
    * @param action the action for each agent
    * @param selectedRoutes for each request, the selected path to assign
    * @param optimalCosts the cost estimate for each path, in the ordering provided by the (ordered) Map[Request, Path]
    * @return the responses, or, an error if the action does not reference one of the included requests
    */
  def toResponses(
    action: Map[AgentId, Int],
    selectedRoutes: Map[Request, Path],
    optimalCosts: List[Cost]
  ): Either[Error, List[Response]] = {
    val result = selectedRoutes.toList.zip(optimalCosts).traverse {
      case ((req, path), cost) =>
        action.get(AgentId(req.agent)) match {
          case None =>
            Left(new Error(s"internal error: failed to build response for request $req, not found in action"))
          case Some(pathIndex) =>
            val edgeList = path.map { _.edgeId }
            val response = Response(req, pathIndex, edgeList, cost)
            Right(response)
        }
    }

    result
  }
}
