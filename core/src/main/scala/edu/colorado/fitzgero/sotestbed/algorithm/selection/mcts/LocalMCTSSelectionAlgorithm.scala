package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import scala.annotation.tailrec
import scala.util.Random

import cats.data.OptionT
import cats.effect.SyncIO
import cats.{Id, Monad}
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward
import cse.bdlab.fitzgero.mcts.core.terminationcriterion.TerminationCriterion
import cse.bdlab.fitzgero.mcts.core.{ActionSelection, BuiltInRandomGenerator, RandomGenerator, RandomSelection}
import cse.bdlab.fitzgero.mcts.tree.MCTreePedrosoReiReward
import cse.bdlab.fitzgero.mcts.variant.PedrosoReiMCTS
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class LocalMCTSSelectionAlgorithm[V, E] (
  seed: Long,
  exhaustiveSearchSampleLimit: Int,
  terminationFunction: SelectionState => Boolean
) extends SelectionAlgorithm[SyncIO, V, E] with LazyLogging {

  var localSeed: Long = seed

  def selectRoutes(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[SyncIO, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[SyncIO, V, E], Path) => SyncIO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): SyncIO[SelectionAlgorithm.Result] = {

    if (alts.isEmpty) SyncIO {
      SelectionAlgorithm.Result(List.empty, Cost.Zero, NonNegativeNumber.Zero)
    } else if (alts.size == 1) SyncIO {

      // select the true shortest path for this agent
      val request: Request = alts.keys.head
      val requestAlts: List[Path] = alts.values.head
      requestAlts match {
        case Nil =>

          // request had no routes!
          SelectionAlgorithm.Result(List.empty, Cost.Zero, NonNegativeNumber.Zero)
        case trueShortestPath :: _ =>

          // get cost estimate of this route
          val trueShortestCost: SelectionCost = SelectionAlgorithm.evaluateCostOfSelection(
            List(trueShortestPath),
            roadNetwork,
            pathToMarginalFlowsFunction,
            combineFlowsFunction,
            marginalCostFunction
          ).unsafeRunSync()

          // package result
          SelectionAlgorithm.Result(
            selectedRoutes = List(Response(request, trueShortestPath.map{_.edgeId}, trueShortestCost.overallCost)),
            estimatedCost = trueShortestCost.overallCost,
            samples = NonNegativeNumber.One
          )
      }
    } else if (SelectionAlgorithm.numCombinationsLessThanThreshold(alts, exhaustiveSearchSampleLimit)) {
      // problem small enough for an exhaustive search
      logger.info(s"performing exhaustive search for ${alts.size} agents")
      SelectionAlgorithm.performExhaustiveSearch(
        alts,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      )
    } else SyncIO {

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

      val samples: NonNegativeNumber = NonNegativeNumber(tree.visits.toInt).getOrElse(NonNegativeNumber.Zero)
      val bestCost: Cost = Cost(mcts.globalBestSimulation.toDouble)

      // package results
      val responses: List[Response] = {
        for {
          (((request, paths), idx), cost) <- alts.zip(mcts.bestSolution).zip(mcts.bestAgentCosts)
        } yield {
          Response(request, paths(idx).map { _.edgeId }, cost)
        }
      }.toList

      // some logging
      val avgAlts: Double = if (alts.isEmpty) 0D else alts.map{case (_, alts) => alts.size}.sum.toDouble / alts.size

      logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: $samples")
      logger.info(s"COST_EST: BEST $bestCost, SELFISH $trueShortestPathsCost, DIFF ${trueShortestPathsCost - bestCost}")

      // update local seed
      this.localSeed = mcts.random.nextInt(Int.MaxValue)

      SelectionAlgorithm.Result(responses, bestCost, samples)
    }
  }

  class PedrosoReiMCTSRouting (
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[SyncIO, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[SyncIO, V, E], Path) => SyncIO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
    terminationFunction: SelectionAlgorithm.SelectionState => Boolean,
    seed: Long,
    startTime: Long) extends PedrosoReiMCTS[Array[Int], Int] with Serializable { pedrosoRei =>
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
    val trueShortestPathSelectionCost: SelectionCost = SelectionAlgorithm.evaluateCostOfSelection(
      PedrosoReiMCTSRouting.stateToSelection(trueShortestPaths, altsInternal).toList,
      roadNetwork,
      pathToMarginalFlowsFunction,
      combineFlowsFunction,
      marginalCostFunction
    ).unsafeRunSync()

    override val objective: UCT_PedrosoRei.Objective = UCT_PedrosoRei.Minimize()
    override var globalBestSimulation: BigDecimal = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
    override var globalWorstSimulation: BigDecimal = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
    override var bestSolution: Array[Int] = trueShortestPaths

    var bestAgentCosts: List[Cost] = trueShortestPathSelectionCost.agentPathCosts

    override def getSearchCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients = {
      UCTScalarPedrosoReiReward.Coefficients(1.0 / math.sqrt(2), globalBestSimulation, globalWorstSimulation)
    }

    override def getDecisionCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients = {
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
        SelectionAlgorithm.evaluateCostOfSelection(
          PedrosoReiMCTSRouting.stateToSelection(state, altsInternal).toList,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        ).unsafeRunSync()

//      logger.debug(s"evaluated state ${state.mkString("[", ", ", "]")} with cost ${selectionCost.overallCost}")

      // underlying MCTS library will set the bestCost itself, but,
      // we must track the agent path costs ourselves
      if (selectionCost.overallCost < Cost(pedrosoRei.globalBestSimulation.toDouble)) {
        this.bestAgentCosts = selectionCost.agentPathCosts
      }

      BigDecimal(selectionCost.overallCost.value)
    }

    override def stateIsNonTerminal(state: Array[Int]): Boolean = state.length < altsInternal.length

    override def selectAction(actions: Seq[Int]): Option[Int] = actionSelection.selectAction(actions)

    override protected val terminationCriterion: TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] =
      new TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]]{

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
                startTime
              )

              // "withinComputationalBudget" has the inverse meaning of a terminationFunction ;-)
              val shouldTerminate: Boolean = terminationFunction(selectionState)
              !shouldTerminate
          }
        }

      }

    override protected def actionSelection: ActionSelection[Array[Int], Int] = RandomSelection(random, generatePossibleActions)

    override val random: RandomGenerator = new BuiltInRandomGenerator(Some{seed})

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
        val action: Int = random.nextInt(numAltsForNextPerson)
        val newState: Array[Int] = addToState(state, action)
        if (numAltsForNextPerson == action) {
          newState
        }
//        logger.debug(s"old state: ${state.mkString("[", ", ", "]")} - new state: ${newState.mkString("[", ", ", "]")}")
//        logger.debug(s"agent ${state.length} selected action ${action} from $numAltsForNextPerson alts")

        newState
      }
    }
  }
}
