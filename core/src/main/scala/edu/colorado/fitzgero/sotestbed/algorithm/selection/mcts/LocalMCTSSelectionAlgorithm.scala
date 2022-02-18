package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import java.io.{FileWriter, PrintWriter}

import scala.util.{Failure, Success, Try}

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import com.typesafe.scalalogging.LazyLogging
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward
import cse.bdlab.fitzgero.mcts.core.terminationcriterion.TerminationCriterion
import cse.bdlab.fitzgero.mcts.core.{ActionSelection, BuiltInRandomGenerator, RandomGenerator, RandomSelection}
import cse.bdlab.fitzgero.mcts.tree.MCTreePedrosoReiReward
import cse.bdlab.fitzgero.mcts.variant.PedrosoReiMCTS
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, TrueShortestSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class LocalMCTSSelectionAlgorithm[V, E](
  seed: Long,
  exhaustiveSearchSampleLimit: Int,
  minimumAverageBatchTravelImprovement: Cost, // todo: wire this value in to batch selection (positive time value here)
  terminationFunction: SelectionState => Boolean
) extends SelectionAlgorithm[IO, V, E]
    with LazyLogging {

  var localSeed: Long = seed

  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, V, E],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, V, E], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
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
    } else
      IO {

        // set up MCTS-based route selection solver
        val startTime: Long = System.currentTimeMillis
        val mcts: PedrosoReiMCTSRouting = new PedrosoReiMCTSRouting(
          alts,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction,
          terminationFunction,
          this.localSeed,
          startTime
        )

        val trueShortestPathsCost: Cost = mcts.trueShortestPathSelectionCost.overallCost

        // run algorithm - SIDE EFFECTS in mcts!
        val tree: mcts.Tree = mcts.run()

        // compute tree shape
//        // hey, maybe we just hard-code this.. val histBins = alts.values.map { _.length }.max
//        val histBins = 11
//        val zeroHist = Array.fill(histBins + 1)(0) // bin counts [0, histBins]
//        def buildHistograms(t: mcts.Tree, minReward: Option[Double] = None): (Array[Int], Array[Int]) = {
//          val doesntMeetRewardMinimum = minReward.exists(_ > t.reward(mcts.getDecisionCoefficients(t)))
//          if (doesntMeetRewardMinimum) {
//            (zeroHist, zeroHist)
//          } else {
//
//            t.children match {
//              case None =>
//                val nodesHist  = zeroHist.updated(0, 1)
//                val visitsHist = zeroHist.updated(0, t.visits.toInt)
//                (nodesHist, visitsHist)
//              case Some(children) =>
//                val histIndex  = children.size
//                val nodesHist  = zeroHist.updated(histIndex, 1)
//                val visitsHist = zeroHist.updated(histIndex, t.visits.toInt)
//                children.values
//                  .map { child => buildHistograms(child(), minReward) }
//                  .foldLeft((nodesHist, visitsHist)) {
//                    case ((nodesAcc, visitsAcc), (nodesChild, visitsChild)) =>
//                      val updatedNodesAcc  = nodesAcc.zip(nodesChild).map { case (a, b)   => a + b }
//                      val updatedVisitsAcc = visitsAcc.zip(visitsChild).map { case (a, b) => a + b }
//                      (updatedNodesAcc, updatedVisitsAcc)
//                  }
//            }
//          }
//        }
//        val (nodesHist, visitsHist) = buildHistograms(tree)
////        println(histogram.zipWithIndex.map { case (c, i)     => s"$i:$c" }.mkString(" "))
////        println(minRewardHist.zipWithIndex.map { case (c, i) => s"$i:$c" }.mkString(" "))
//        val treeNodes         = nodesHist.sum
//        val leaves            = nodesHist(0)
//        val branches          = nodesHist.tail.sum
//        val treeEdgeCount     = nodesHist.zipWithIndex.map { case (a, b) => a * b }.sum.toDouble
//        val avgBranchChildren = treeEdgeCount / branches
//        val avgChildren       = treeEdgeCount / treeNodes
//        val hist              = nodesHist.mkString(",")
//        val avgVisitsHist = nodesHist
//          .zip(visitsHist)
//          .map { case (n, v) => f"${if (n == 0) 0.0 else v.toDouble / n}%.2f" }
//          .mkString(",")
//
//        val treeStats =
//          f",${alts.size},$treeNodes,$leaves,$branches,$avgChildren%.2f,$avgBranchChildren%.2f,${tree.visits},$hist,$avgVisitsHist\n"
//        val printWriter: PrintWriter = new PrintWriter(new FileWriter("treeStats.csv", true))
//        printWriter.append(treeStats)
//        printWriter.close()

        val samples: NonNegativeNumber = NonNegativeNumber(tree.visits.toInt).getOrElse(NonNegativeNumber.Zero)
        val bestCost: Cost             = Cost(mcts.globalBestSimulation.toDouble)

        // package results
        val responses: List[Response] = {
          for {
            (((request, paths), idx), cost) <- alts.zip(mcts.bestSolution).zip(mcts.bestAgentCosts)
          } yield {
            Response(request, idx, paths(idx).map { _.edgeId }, cost)
          }
        }.toList

        // some logging
        val avgAlts: Double =
          if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
        val travelTimeDiff: Cost     = bestCost - trueShortestPathsCost
        val meanTravelTimeDiff: Cost = Cost((bestCost - trueShortestPathsCost).value / alts.size)

        logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: $samples")
        logger.info(
          f"COST_EST: BEST $bestCost, SELFISH $trueShortestPathsCost, DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
        )

        // update local seed
        this.localSeed = mcts.random.nextInt(Int.MaxValue)

        val result = SelectionAlgorithm.SelectionAlgorithmResult(
          selectedRoutes = responses,
          estimatedCost = bestCost,
          selfishCost = trueShortestPathsCost,
          travelTimeDiff = travelTimeDiff,
          averageTravelTimeDiff = meanTravelTimeDiff,
          samples = samples
        )

        result
      }
  }

  class PedrosoReiMCTSRouting(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, V, E], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
    terminationFunction: SelectionAlgorithm.SelectionState => Boolean,
    seed: Long,
    startTime: Long
  ) extends PedrosoReiMCTS[Array[Int], Int]
      with Serializable { pedrosoRei =>
    // state? a list of selected alternatives (indices) : List[Int] (in reverse order would be smart)
    // action: another index : Int

    val (altsInternal: Array[Array[Path]], personIndexToRequest: Array[Request]) =
      alts.foldLeft((Array.empty[Array[Path]], Array.empty[Request])) {
        case ((altsInternal, mapTo), (req, paths)) =>
          (
            altsInternal :+ paths.toArray,
            mapTo :+ req
          )
      }
    val trueShortestPaths: Array[Int] = Array.fill(altsInternal.length)(0)

    val trueShortestPathSelectionCost: SelectionCost = SelectionAlgorithm
      .evaluateCostOfSelection(
        PedrosoReiMCTSRouting.stateToSelection(trueShortestPaths, altsInternal).toList,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      )
      .unsafeRunSync()

    override val objective: UCT_PedrosoRei.Objective = UCT_PedrosoRei.Minimize()
    override var globalBestSimulation: BigDecimal    = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
    override var globalWorstSimulation: BigDecimal   = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
    override var bestSolution: Array[Int]            = trueShortestPaths

    val searchSpaceSize: BigDecimal = altsInternal.map { paths => BigDecimal(paths.length) }.product

    var bestAgentCosts: List[Cost] = trueShortestPathSelectionCost.agentPathCosts

    override def getSearchCoefficients(
      tree: MCTreePedrosoReiReward[Array[Int], Int]
    ): UCTScalarPedrosoReiReward.Coefficients = {
      UCTScalarPedrosoReiReward.Coefficients(1.0 / math.sqrt(2), globalBestSimulation, globalWorstSimulation)
    }

    override def getDecisionCoefficients(
      tree: MCTreePedrosoReiReward[Array[Int], Int]
    ): UCTScalarPedrosoReiReward.Coefficients = {
      UCTScalarPedrosoReiReward.Coefficients(0, globalBestSimulation, globalWorstSimulation)
    }

    override def startState: Array[Int] = Array()

    override def generatePossibleActions(state: Array[Int]): Seq[Int] = {
      if (state.length == altsInternal.length) {
        logger.error(s"attempting to generate an action on a completed state: ${state.mkString("[", ", ", "]")}")
        state
      } else {
        altsInternal(state.length).indices
      }
    }

    override def applyAction(state: Array[Int], action: Int): Array[Int] =
      PedrosoReiMCTSRouting.addToState(state, action)

    override def evaluateTerminal(state: Array[Int]): BigDecimal = {
      val selectionCost: SelectionCost =
        SelectionAlgorithm
          .evaluateCostOfSelection(
            PedrosoReiMCTSRouting.stateToSelection(state, altsInternal).toList,
            roadNetwork,
            pathToMarginalFlowsFunction,
            combineFlowsFunction,
            marginalCostFunction
          )
          .unsafeRunSync()

      // underlying MCTS library will set the bestCost itself, but,
      // we must track the agent path costs ourselves
      if (selectionCost.overallCost < Cost(pedrosoRei.globalBestSimulation.toDouble)) {
        this.bestAgentCosts = selectionCost.agentPathCosts
      }

      BigDecimal(selectionCost.overallCost.value)
    }

    override def stateIsNonTerminal(state: Array[Int]): Boolean = state.length < altsInternal.length

    override def selectAction(actions: Seq[Int]): Option[Int] = actionSelection.selectAction(actions)

    override protected val terminationCriterion
      : TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] =
      new TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] {

        def init(): Unit = ()

        def withinComputationalBudget(tree: pedrosoRei.Tree): Boolean = {
          NonNegativeNumber(tree.visits.toInt) match {
            case Left(e) =>
              logger.error(e.getMessage)
              false
            case Right(samples) =>
              val selectionState = SelectionState(
                bestSolution.toList,
                Cost(globalBestSimulation.toDouble),
                pedrosoRei.bestAgentCosts,
                samples,
                searchSpaceSize,
                startTime
              )

              // "withinComputationalBudget" has the inverse meaning of a terminationFunction ;-)
              val shouldTerminate: Boolean = terminationFunction(selectionState)

              logger.whenDebugEnabled {
                if (shouldTerminate) {
                  val timeElapsed = f"${(System.currentTimeMillis - startTime).toDouble / 1000}%.2f"
                  logger.debug(s"terminating after $samples samples, $timeElapsed seconds")
                }
              }
              !shouldTerminate
          }
        }

      }

    override protected def actionSelection: ActionSelection[Array[Int], Int] =
      RandomSelection(random, generatePossibleActions)

    override val random: RandomGenerator = new BuiltInRandomGenerator(Some { seed })

//    logger.debug("finished constructing PedrosoReiMCTSRouting")
  }

  object PedrosoReiMCTSRouting {

    def addToState(state: Array[Int], action: Int): Array[Int] = state :+ action

    def stateToSelection(state: Array[Int], alts: Array[Array[Path]]): Array[Path] = {
      val selection: Array[Path] = for {
        (altIdx, personIdx) <- state.zipWithIndex
      } yield {
        alts(personIdx)(altIdx)
      }
      selection
    }

    def selectRandomNextAction(state: Array[Int], alts: Array[Array[Path]], random: RandomGenerator): Array[Int] = {
      if (state.length >= alts.length) {
        // state has a selection for each agent
        state
      } else {
        // make a selection for the next agent
        val numAltsForNextPerson: Int = alts(state.length).length
        val action: Int               = random.nextInt(numAltsForNextPerson)
        val newState: Array[Int]      = addToState(state, action)

        newState
      }
    }
  }
}
