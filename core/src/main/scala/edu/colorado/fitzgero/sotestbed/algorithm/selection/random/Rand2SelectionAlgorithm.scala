package edu.colorado.fitzgero.sotestbed.algorithm.selection.random

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.util.Random

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.algorithm.selection.random.ComputeBudgetFunctionOps.ComputeBudgetFunctionOpsInstance
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, SelectionAlgorithmHelper}
import edu.colorado.fitzgero.sotestbed.config.SelectionComputeBudgetFunctionConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class Rand2SelectionAlgorithm(
  seed: Long,
  exhaustiveSearchSampleLimit: Int,
  computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
  computeBudgetTestRate: Int
) extends SelectionAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  import SelectionAlgorithm._

  val random: Random = new Random(seed)

  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

    logger.debug(s"selectRoutes called with ${alts.size} requests")

    if (alts.isEmpty) {
      IO {
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
        .performExhaustiveSearch[IO, Coordinate, EdgeBPR](
          alts,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
        .flatMap { result =>
          val avgAlts: Double =
            if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
          logger.info(
            f"AGENTS: ${result.selectedRoutes.length} AVG_ALTS: $avgAlts%.2f SAMPLES: ${result.samples} - EXHAUSTIVE SEARCH"
          )
          logger.info(
            f"COST_EST: BEST ${result.estimatedCost}, SELFISH ${result.selfishCost}, DIFF ${result.estimatedCost - result.selfishCost} AVG_DIFF ${(result.estimatedCost - result.selfishCost).value / alts.size}%.2f"
          )
          IO {
            result
          }
        }
    } else {

      SelectionAlgorithmHelper(roadNetwork, alts, agentOrdering = None, pathOrdering = None).map { sah =>
        // build assets for search
        // this involves interpreting the requests and their alternative paths into something which
        // can be represented as a scala.collection.BitSet.
        val startTime: Long = System.currentTimeMillis

        val random                = new Random(seed)
        val problemRepresentation = MultiChoice(sah.choices)
        val searchSpaceSize       = alts.values.map { a => BigDecimal(a.size) }.product
        val evalFn: MultiChoiceState => Double =
          (mcs: MultiChoiceState) => {
            val selection = mcs.getChoices(problemRepresentation).takeWhile(_.nonEmpty).flatten
            sah.batchMarginalCost(selection, marginalCostFunction)
          }
        val initialState =
          problemRepresentation.actions.indices.foldLeft(BitSet.empty) { (acc, idx) =>
            acc.addChoiceToState(problemRepresentation.statePartitionOffsets, idx, 0)
          }
        val initialCost = evalFn(initialState)
        val computeFn   = computeBudgetFunctionConfig.makeComputeBudgetFn(searchSpaceSize)

        // run a tail recursive random search, writing the results in-place for highest performance
        var bestState = initialState
        var bestCost  = initialCost

        @tailrec
        def run(iterations: Int = 1): Int = {

          val stopCondition = if (iterations % computeBudgetTestRate == 0) {
            !computeFn(iterations, startTime) // this function is a "continue" function, so we flip it
          } else {
            false
          }
          if (stopCondition) {
            iterations
          } else {

            // pick a random assignment
            val randomState = {
              problemRepresentation.actions.indices.foldLeft(BitSet.empty) { (acc, idx) =>
                val action = random.nextInt(problemRepresentation.actions(idx))
                acc.addChoiceToState(problemRepresentation.statePartitionOffsets, idx, action)
              }
            }
            val randomCost = evalFn(randomState)

            // update if randomly-selected result is better than our previous best
            if (randomCost < bestCost) {
              bestCost = randomCost
              bestState = randomState
            }

            run(iterations + 1)
          }
        }

        // run search

        val samples = run()
        val runTime = f"${(System.currentTimeMillis - startTime).toDouble / 1000.toDouble}%.2f"

        // collect results
        val searchSpaceExplored = (BigDecimal(samples) / searchSpaceSize).toDouble
        val bestSelection       = bestState.getChoices(problemRepresentation).flatten
        val overallCost         = Cost(sah.batchMarginalCost(bestSelection, marginalCostFunction))
        val agentCosts          = sah.agentMarginalCosts(bestSelection, marginalCostFunction).map { Cost.apply }
        val tspCost             = Cost(initialCost)

        logger.info(
          f"finished search in $runTime seconds with ${searchSpaceExplored * 100}%.6f%% search space explored"
        )

        val responses = for {
          (pathIdx, reqIdx) <- bestSelection.zipWithIndex.toList
          req  = sah.requestsLookup(reqIdx)
          path = sah.pathsLookup(reqIdx)(pathIdx)
          cost = agentCosts(reqIdx)
        } yield {
          Response(
            request = req,
            pathIndex = pathIdx,
            path.map { _.edgeId },
            Cost(cost)
          )
        }

        val avgAlts: Double =
          if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
        val travelTimeDiff     = overallCost - tspCost
        val meanTravelTimeDiff = Cost((overallCost.value - tspCost.value) / alts.size.toDouble)

        logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: $samples")
        logger.info(
          f"COST_EST: BEST $overallCost, SELFISH ${tspCost.value}%.2f, " +
            f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
        )

        if (responses.length > 20) {
          println("foo")
        }

        val result = SelectionAlgorithmResult(
          selectedRoutes = responses,
          estimatedCost = overallCost,
          selfishCost = tspCost,
          travelTimeDiff = travelTimeDiff,
          averageTravelTimeDiff = meanTravelTimeDiff,
          samples = NonNegativeNumber(samples).toOption.get,
          ratioOfSearchSpaceExplored = searchSpaceExplored
        )

        result

      }
    }
  }
}
