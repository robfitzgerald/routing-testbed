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
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionState
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class LocalMCTSSelectionAlgorithm[V, E] extends SelectionAlgorithm[SyncIO, V, E] with LazyLogging {

  def selectRoutes(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[SyncIO, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[SyncIO, V, E], Path) => SyncIO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
    terminationFunction: SelectionAlgorithm.SelectionState => Boolean
  ): SyncIO[SelectionAlgorithm.Result] = {

    ???
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

    val objective: UCT_PedrosoRei.Objective = UCT_PedrosoRei.Minimize()

    var globalBestSimulation: BigDecimal = objective.defaultBest
    var globalWorstSimulation: BigDecimal = objective.defaultWorst
    var bestSolution: Array[Int] = trueShortestPaths

    def getSearchCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients =
      UCTScalarPedrosoReiReward.Coefficients(1.0 / math.sqrt(2), globalBestSimulation, globalWorstSimulation)

    def getDecisionCoefficients(tree: MCTreePedrosoReiReward[Array[Int], Int]): UCTScalarPedrosoReiReward.Coefficients =
      UCTScalarPedrosoReiReward.Coefficients(0, globalBestSimulation, globalWorstSimulation)


    def generatePossibleActions(state: Array[Int]): Seq[Int] = {

      @tailrec
      def _gen(partialState: Array[Int]): Array[Int] = {
        if (partialState.length == altsInternal.length) partialState
        else _gen(PedrosoReiMCTSRouting.selectRandomNextAction(partialState, altsInternal, random))
      }

      _gen(state)
    }

    def applyAction(state: Array[Int], action: Int): Array[Int] =
      PedrosoReiMCTSRouting.addToState(state, action)

    def evaluateTerminal(state: Array[Int]): BigDecimal = {
      val marginalFlowsF: SyncIO[List[List[(EdgeId, Flow)]]] =
        PedrosoReiMCTSRouting
          .stateToSelection(state, altsInternal)
          .toList
          .traverse{pathToMarginalFlowsFunction(roadNetwork, _)}


      val costIO: SyncIO[Cost] =
        marginalFlowsF
          .flatMap{ marginalFlows =>
            marginalFlows
              .flatten
              .groupBy{_._1}
              .toList
              .traverse{ case (edgeId, flows) =>
                for {
                  edgeAndAttribute <- roadNetwork.edge(edgeId)
                } yield {
                  edgeAndAttribute match {
                    case None => Cost.Zero
                    case Some(RoadNetwork.EdgeIdAndAttribute(_, edge)) =>
                      val combinedFlows: Flow = combineFlowsFunction(flows.map{_._2})
                      marginalCostFunction(edge)(combinedFlows)
                  }
                }
              }.map{
                _.foldLeft(Cost.Zero){_ + _}
              }
          }

      BigDecimal(costIO.unsafeRunSync().value)
    }

    def stateIsNonTerminal(state: Array[Int]): Boolean = state.length < altsInternal.length

    def selectAction(actions: Seq[Int]): Option[Int] = actionSelection.selectAction(actions)

    def startState: Array[Int] = trueShortestPaths

    protected val terminationCriterion: TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]] =
      new TerminationCriterion[Array[Int], Int, MCTreePedrosoReiReward[Array[Int], Int]]{

        def init(): Unit = ()

        def withinComputationalBudget(tree: pedrosoRei.Tree): Boolean = {
          NaturalNumber(tree.visits.toInt) match {
            case Left(e) =>
              logger.error(e.getMessage)
              false
            case Right(nat) =>
              val selectionState = SelectionState(
                bestSolution.toList,
                Cost(globalBestSimulation.toDouble),
                nat,
                startTime
              )
              terminationFunction(selectionState)
          }
        }
      }

    protected def actionSelection: ActionSelection[Array[Int], Int] = RandomSelection(random, generatePossibleActions)

    protected def random: RandomGenerator = new BuiltInRandomGenerator(Some{seed})
  }

  object PedrosoReiMCTSRouting {

    def addToState(state: Array[Int], action: Int): Array[Int] = state :+ action

    def stateToSelection(state: Array[Int], alts: Array[Array[Path]]): Array[Path] =
      for {
        (altIdx, personIdx) <- state.zipWithIndex
      } yield {
        alts(personIdx)(altIdx)
      }

    def selectRandomNextAction(state: Array[Int], alts: Array[Array[Path]], random: RandomGenerator): Array[Int] = {
      if (state.length < alts.length) {
        val numAltsForNextPerson = alts(state.length).length
        addToState(state, random.nextInt(numAltsForNextPerson))
      } else {
        state
      }
    }
  }
}
