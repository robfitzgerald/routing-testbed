//package edu.colorado.fitzgero.sotestbed.algorithm.batching
//
//import cats.Monad
//import cats.effect.SyncIO
//
//import com.typesafe.scalalogging.LazyLogging
//import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei
//import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward
//import cse.bdlab.fitzgero.mcts.core.{ActionSelection, BuiltInRandomGenerator, RandomGenerator, RandomSelection}
//import cse.bdlab.fitzgero.mcts.core.terminationcriterion.TerminationCriterion
//import cse.bdlab.fitzgero.mcts.tree.MCTreePedrosoReiReward
//import cse.bdlab.fitzgero.mcts.variant.PedrosoReiMCTS
//import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
//import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
//import edu.colorado.fitzgero.sotestbed.model.agent.Request
//import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, NonNegativeNumber, SimTime}
//import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
//import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
//import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
//
//class MCTSMatchingTSPBatching extends BatchingFunction with LazyLogging {
//
//  /**
//    * takes the current batching strategy and any updates about replan-able agents, and spits out an
//    * update to that batching strategy
//    *
//    * @param roadNetwork         the current road network state
//    * @param activeRouteRequests agents which are available for SO routing requests
//    * @param currentTime         the current sim time
//    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
//    */
//  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
//                                                activeRouteRequests: List[AgentBatchData.RouteRequestData],
//                                                currentTime: SimTime): F[Option[List[(String, List[Request])]]] = {
//
//    // community search.
//
//    // if a link exists once in a grouping, then it contributes nothing to the hill climbing exercise
//
//    // for any link shared by n > 2 or more agents in a group, we contribute
//    // a value equal to the complete graph on those agents (n choose 2, or (n*n-1)/2)
//
//    // what is our "state space"? for each agent (index), we choose a grouping number. by default,
//    // each agent is assigned to it's own grouping. a random action is to re-assign an agent's grouping.
//
//    ???
//  }
//}
//
//object MCTSMatchingTSPBatching {
//
//  class PedrosoReiMCTSBatchingSearch(
//    alts: Map[Request, List[Path]],
//    roadNetwork: RoadNetwork[SyncIO, Coordinate, EdgeBPR],
//  ) extends PedrosoReiMCTS[Array[Int], Int]
//      with LazyLogging { pedrosoRei =>
//
//    val (altsInternal: Array[Array[Path]], personIndexToRequest: Array[Request]) =
//      alts.foldLeft((Array.empty[Array[Path]], Array.empty[Request])) {
//        case ((altsInternal, mapTo), (req, paths)) =>
//          (
//            altsInternal :+ paths.toArray,
//            mapTo :+ req
//          )
//      }
//    val trueShortestPaths: Array[Int] = Array.fill(altsInternal.length)(0)
//
//    val trueShortestPathSelectionCost: SelectionCost = SelectionAlgorithm
//      .evaluateCostOfSelection(
//        PedrosoReiMCTSRouting.stateToSelection(trueShortestPaths, altsInternal).toList,
//        roadNetwork,
//        pathToMarginalFlowsFunction,
//        combineFlowsFunction,
//        marginalCostFunction
//      )
//      .unsafeRunSync()
//
//    override val objective: UCT_PedrosoRei.Objective = UCT_PedrosoRei.Minimize()
//    override var globalBestSimulation: BigDecimal    = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
//    override var globalWorstSimulation: BigDecimal   = BigDecimal(trueShortestPathSelectionCost.overallCost.value)
//    override var bestSolution: Array[Int]            = trueShortestPaths
//
//    var bestAgentCosts: List[Cost] = trueShortestPathSelectionCost.agentPathCosts
//
//    override def getSearchCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients = {
//      UCTScalarPedrosoReiReward.Coefficients(1.0 / math.sqrt(2), globalBestSimulation, globalWorstSimulation)
//    }
//
//    override def getDecisionCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients = {
//      UCTScalarPedrosoReiReward.Coefficients(0, globalBestSimulation, globalWorstSimulation)
//    }
//
//    override def startState: Array[Int] = Array()
//
//    override def generatePossibleActions(state: Array[Int]): Seq[Int] = {
//      if (state.length == altsInternal.length) {
//        logger.error(s"attempting to generate an action on a completed state: ${state.mkString("[", ", ", "]")}")
//        state
//      } else {
//        altsInternal(state.length).indices
//      }
//    }
//
//    override def applyAction(state: Array[Int], action: Int): Array[Int] =
//      PedrosoReiMCTSRouting.addToState(state, action)
//
//    override def evaluateTerminal(state: Array[Int]): BigDecimal = {
//      val selectionCost: SelectionCost =
//        SelectionAlgorithm
//          .evaluateCostOfSelection(
//            PedrosoReiMCTSRouting.stateToSelection(state, altsInternal).toList,
//            roadNetwork,
//            pathToMarginalFlowsFunction,
//            combineFlowsFunction,
//            marginalCostFunction
//          )
//          .unsafeRunSync()
//
//      //      logger.debug(s"evaluated state ${state.mkString("[", ", ", "]")} with cost ${selectionCost.overallCost}")
//
//      // underlying MCTS library will set the bestCost itself, but,
//      // we must track the agent path costs ourselves
//      if (selectionCost.overallCost < Cost(pedrosoRei.globalBestSimulation.toDouble)) {
//        this.bestAgentCosts = selectionCost.agentPathCosts
//      }
//
//      BigDecimal(selectionCost.overallCost.value)
//    }
//
//    override def stateIsNonTerminal(state: Array[Int]): Boolean = state.length < altsInternal.length
//
//    override def selectAction(actions: Seq[Int]): Option[Int] = actionSelection.selectAction(actions)
//
//    override protected val terminationCriterion: TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] =
//      new TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] {
//
//        def init(): Unit = ()
//
//        def withinComputationalBudget(tree: pedrosoRei.Tree): Boolean = {
//          NonNegativeNumber(tree.visits.toInt) match {
//            case Left(e) =>
//              logger.error(e.getMessage)
//              false
//            case Right(samples) =>
//              val selectionState = SelectionState(
//                bestSolution.toList,
//                Cost(globalBestSimulation.toDouble),
//                pedrosoRei.bestAgentCosts,
//                samples,
//                startTime
//              )
//
//              // "withinComputationalBudget" has the inverse meaning of a terminationFunction ;-)
//              val shouldTerminate: Boolean = terminationFunction(selectionState)
//              !shouldTerminate
//          }
//        }
//
//      }
//
//    override protected def actionSelection: ActionSelection[Array[Int], Int] = RandomSelection(random, generatePossibleActions)
//
//    override val random: RandomGenerator = new BuiltInRandomGenerator(Some { seed })
//
//  }
//}
