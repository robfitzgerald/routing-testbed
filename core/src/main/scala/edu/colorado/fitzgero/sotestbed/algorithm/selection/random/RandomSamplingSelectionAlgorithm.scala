package edu.colorado.fitzgero.sotestbed.algorithm.selection.random

import scala.util.Random

import cats.Monad
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class RandomSamplingSelectionAlgorithm[F[_]: Monad, V, E](
  seed: Long,
  exhaustiveSearchSampleLimit: Int,
  terminationFunction: SelectionState => Boolean,
  selectionAcceptanceFunction: SelectionAlgorithm.SelectionAlgorithmResult => Boolean
) extends SelectionAlgorithm[F, V, E]
    with LazyLogging {

  import SelectionAlgorithm._

  val random: Random = new Random(seed)

  def selectRoutes(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): F[SelectionAlgorithm.SelectionAlgorithmResult] = {

    logger.debug(s"selectRoutes called with ${alts.size} requests")

    val startTime: Long = System.currentTimeMillis

    if (alts.isEmpty) {
      Monad[F].pure {
        SelectionAlgorithm.SelectionAlgorithmResult()
      }
    } else if (alts.toList.lengthCompare(1) == 0 && alts.values.head.nonEmpty) {
      SelectionAlgorithm
        .evaluateCostOfSelection(
          alts.values.flatMap { _.headOption }.toList,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
        .map { selectionCost =>
          SelectionAlgorithm.SelectionAlgorithmResult(
            selectedRoutes = alts.map {
              case (req, paths) => Response(req, 0, paths.head.map { _.edgeId }, selectionCost.overallCost)
            }.toList,
            estimatedCost = selectionCost.overallCost,
            selfishCost = selectionCost.overallCost,
            samples = NonNegativeNumber.One
          )
        }
    } else if (SelectionAlgorithm.numCombinationsLessThanThreshold(alts, exhaustiveSearchSampleLimit)) {
      // problem small enough for an exhaustive search
      SelectionAlgorithm
        .performExhaustiveSearch(
          alts,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
        .flatMap { result =>
          if (selectionAcceptanceFunction(result)) {
            val avgAlts: Double =
              if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
            logger.info(
              f"AGENTS: ${result.selectedRoutes.length} AVG_ALTS: $avgAlts%.2f SAMPLES: ${result.samples} - EXHAUSTIVE SEARCH"
            )
            logger.info(
              f"COST_EST: BEST ${result.estimatedCost}, SELFISH ${result.selfishCost}, DIFF ${result.estimatedCost - result.selfishCost} AVG_DIFF ${(result.estimatedCost - result.selfishCost).value / alts.size}%.2f"
            )
            Monad[F].pure {
              result
            }
          } else {
            logger.info(s"dismissing result due to selection acceptance policy")
            Monad[F].pure {
              SelectionAlgorithm.SelectionAlgorithmResult()
            }
          }
        }
    } else {
      // turn path alternatives into vector for faster indexing performance
      val indexedAlts: Map[Request, Vector[Path]] =
        alts.map { tuple => tuple._1 -> tuple._2.toVector }

      // selfish assignment is assume to be the first alternative for each request
      val selfishAssignmentSelection: List[Int] = alts.toList.map { _ => 0 }

      // evaluate selfish assignment cost
      val selfishAssignmentCostF: F[SelectionCost] =
        SelectionAlgorithm.evaluateCostOfSelection(
          alts.values.flatMap { _.headOption }.toList,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )

      // run iterative sampling using selfish as the start state
      for {
        selfishCostPayload <- selfishAssignmentCostF
        selfishCost     = selfishCostPayload.overallCost
        searchSpaceSize = BigDecimal(indexedAlts.values.map { _.size.toLong }.product)
        startState = SelectionState(
          bestSelectionIndices = selfishAssignmentSelection,
          bestOverallCost = selfishCost,
          agentPathCosts = selfishCostPayload.agentPathCosts,
          samples = NonNegativeNumber.One,
          searchSpaceSize = searchSpaceSize,
          startTime = startTime
        )
        endState <- RandomSamplingSelectionOps.performRandomSampling(
          startState,
          indexedAlts.values,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction,
          terminationFunction,
          random
        )

      } yield {

        // re-cast as Response objects
        val responses: List[Response] =
          indexedAlts
            .zip(endState.bestSelectionIndices)
            .zip(endState.agentPathCosts)
            .map {
              case (((request, alts), idx), cost) =>
                Response(request, idx, alts(idx).map { _.edgeId }, cost)
            }
            .toList
        val travelTimeDiff: Cost     = endState.bestOverallCost - selfishCost
        val meanTravelTimeDiff: Cost = Cost((endState.bestOverallCost - selfishCost).value / alts.size)

//        val bestCostStr: String = endState.bestOverallCost.toString.padTo(10, ' ')
        logger.info(s"AGENTS: ${responses.length} SAMPLES: ${endState.samples}")
        logger.info(
          f"COST_EST: BEST ${endState.bestOverallCost}, SELFISH $selfishCost, DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
        )

        // final return value
        val result = SelectionAlgorithm.SelectionAlgorithmResult(
          selectedRoutes = responses,
          estimatedCost = endState.bestOverallCost,
          selfishCost = selfishCost,
          travelTimeDiff = travelTimeDiff,
          averageTravelTimeDiff = meanTravelTimeDiff,
          samples = endState.samples,
          ratioOfSearchSpaceExplored = endState.ratioOfSearchSpaceExplored
        )

        if (selectionAcceptanceFunction(result)) {
          result
        } else {
          logger.info(
            f"dismissing result due to selection acceptance policy (explored ${result.ratioOfSearchSpaceExplored * 100.0}%.2f%% of search space)"
          )
          SelectionAlgorithm.SelectionAlgorithmResult()
        }
      }
    }
  }
}
