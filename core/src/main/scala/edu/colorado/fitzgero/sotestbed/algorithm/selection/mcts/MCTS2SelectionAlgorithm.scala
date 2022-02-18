package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import scala.util.Random

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.scalalogging.LazyLogging
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction.PedrosoRei
import edu.colorado.fitzgero.sotestbed.algorithm.selection.PathOrdering.TravelTimeAscending
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionAlgorithmResult
import edu.colorado.fitzgero.sotestbed.algorithm.selection._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.config.SelectionComputeBudgetFunctionConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class MCTS2SelectionAlgorithm(
  defaultPolicy: DefaultPolicy,
  expandPolicy: ExpandPolicy,
  agentOrdering: Option[AgentOrdering],
  pathOrdering: Option[PathOrdering],
  runnerType: RunnerType,
  mctsCoefficient: Option[Double],
  seed: Long,
  exhaustiveSearchSampleLimit: Int,
  computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
  computeBudgetTestRate: Int
) extends SelectionAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  val localRandom: Random = new Random(seed)

  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

    if (alts.isEmpty) IO {
      SelectionAlgorithm.SelectionAlgorithmResult()
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
        .map { result =>
          val avgAlts: Double =
            if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
          logger.info(
            f"AGENTS: ${result.selectedRoutes.length} AVG_ALTS: $avgAlts%.2f SAMPLES: ${result.samples} - EXHAUSTIVE SEARCH"
          )
          logger.info(
            f"COST_EST: BEST ${result.estimatedCost}, SELFISH ${result.selfishCost}, DIFF ${result.estimatedCost - result.selfishCost} AVG_DIFF ${(result.estimatedCost - result.selfishCost).value / alts.size}%.2f"
          )
          result
        }
    } else {

      val agentOrderingInstance: Option[Ordering[(Request, List[Path])]] = agentOrdering.map {
        case bp: AgentOrdering.BatchProportional => bp.ordering(alts)
      }
      val pathOrderingInstance: Option[Ordering[Path]] = pathOrdering.map {
        case PathOrdering.TravelTimeAscending =>
          PathOrdering.TravelTimeAscending.ordering
        case bp: PathOrdering.BatchProportionalOverlapCount =>
          bp.ordering(alts)
      }

      SelectionAlgorithmHelper(
        roadNetwork,
        alts,
        agentOrderingInstance,
        pathOrderingInstance
      ).flatMap { sah =>
        // build assets for search
        // this involves interpreting the requests and their alternative paths into something which
        // can be represented as a scala.collection.BitSet.
        val problemRepresentation = MultiChoice(sah.choices)
        val searchSpaceSize       = alts.values.map { a => BigDecimal(a.size) }.product

        // the functions for tree traversal and evaluation which will be used in the search.
        val evalFn: MultiChoiceState => Double =
          (mcs: MultiChoiceState) => {
            val selection = mcs.getChoices(problemRepresentation).takeWhile(_.nonEmpty).flatten
            sah.batchMarginalCost(selection, marginalCostFunction)
          }

        // default value is 2 * sqrt(2)
        val mctsCoefficientValue = mctsCoefficient.getOrElse(2.0 / math.sqrt(2.0))

        for {
          bestPair <- runnerType.run(
            choices = sah.choices,
            evalFn = evalFn,
            defFn = defaultPolicy.policy(problemRepresentation, sah),
            valFnConstructor = () => new PedrosoRei(mctsCoefficientValue),
            expandFn = expandPolicy.expand,
            computeBudgetFunctionConfig = computeBudgetFunctionConfig,
            computeBudgetTestRate = computeBudgetTestRate
          )
        } yield {

          // return best selection observed along with stats on the costs + samples
          val (valFn, root) = bestPair
          val prValFn: PedrosoRei = valFn match {
            case rei: PedrosoRei => rei
          }
          val samples = root.observations.visits

          val searchSpaceExplored = (BigDecimal(samples) / searchSpaceSize).toDouble
          val bestSelection       = prValFn.globalMinState.getChoices(problemRepresentation).flatten

          val overallCost = Cost(sah.batchMarginalCost(bestSelection, marginalCostFunction))
          val agentCosts  = sah.agentMarginalCosts(bestSelection, marginalCostFunction).map { Cost.apply }

          val trueShortestPaths: List[Path] = for {
            reqPaths <- sah.pathsLookup.toList
          } yield {
            if (reqPaths.isEmpty) List() else reqPaths.min(TravelTimeAscending.ordering)
          }

          val tspCosts: SelectionAlgorithm.SelectionCost = SelectionAlgorithm
            .evaluateCostOfSelection(
              trueShortestPaths,
              roadNetwork,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            )
            .unsafeRunSync()

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
          val travelTimeDiff     = overallCost - tspCosts.overallCost
          val meanTravelTimeDiff = Cost((overallCost - tspCosts.overallCost).value / alts.size)

          logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: $samples")
          logger.info(
            f"COST_EST: BEST $overallCost, SELFISH ${tspCosts.overallCost}, " +
              f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
          )

          val result = SelectionAlgorithmResult(
            selectedRoutes = responses,
            estimatedCost = overallCost,
            selfishCost = tspCosts.overallCost,
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
}
