package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.rllib.{Action, Observation, RLlibPolicyClient, Reward}

final class RLSelectionAlgorithm(
  client: RLlibPolicyClient,
  encodeObservation: (RoadNetwork[IO, Coordinate, EdgeBPR], Map[Request, List[Path]]) => Observation,
  decodeAction: (Action, Map[Request, List[Path]]) => List[Int],
  computeReward: (SelectionCost, SelectionCost, Map[Request, List[Path]]) => Reward
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
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
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

    val observation = encodeObservation(roadNetwork, alts)

    // query the client to get an action
    val result: IO[SelectionAlgorithm.SelectionAlgorithmResult] = for {
      selfishCostPayload <- costFn(alts.values.flatMap { _.headOption }.toList)
      response           <- client.getAction(observation)
      selectedRouteIdxs = decodeAction(response.action, alts)
      selectedRoutes = alts.values
        .zip(selectedRouteIdxs)
        .map { case (paths, idx) => (idx, paths.toVector.apply(idx)) }
        .toList
      optimalCostPayload <- costFn(selectedRoutes.map { case (_, path) => path })
      optimalCost = optimalCostPayload.overallCost
      reward      = computeReward(selfishCostPayload, optimalCostPayload, alts)
      info        = Map.empty[String, String]
      done        = None
      _ <- client.logReturns(reward, info, done)
    } yield {

      val selfishCost = selfishCostPayload.overallCost

//      val searchSpaceSize = alts.values.map { l => BigDecimal(l.size) }.product
      val avgAlts: Double =
        if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
      val travelTimeDiff: Cost     = optimalCost - selfishCost
      val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)

      // encode the selected actions as a set of responses
      val responses = alts
        .zip(selectedRoutes)
        .zip(optimalCostPayload.agentPathCosts)
        .map {
          case (((req, _), (idx, path)), cost) =>
            Response(req, idx, path.map { _.edgeId }, cost)
        }
        .toList

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
}
