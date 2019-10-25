package edu.colorado.fitzgero.sotestbed.algorithm.selection

import scala.util.Random

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionState
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NaturalNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class RandomSamplingSelectionAlgorithm[F[_]: Monad, V, E](
    seed: Long
) extends SelectionAlgorithm[F, V, E] {

  import SelectionAlgorithm._

  val random: Random = new Random(seed)

  case class EdgeAndMarginalFlow(edgeId: EdgeId, flow: Flow)

  def selectRoutes(
      alts: Map[Request, List[Path]],
      roadNetwork: RoadNetwork[F, V, E],
      pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      marginalCostFunction: E => Flow => Cost,
      terminationFunction: SelectionState => Boolean
  ): F[SelectionAlgorithm.Result] = {

    val startTime: Long = System.currentTimeMillis

    // turn path alternatives into vector for faster indexing performance
    val indexedAlts: Map[Request, Vector[Path]] =
      alts.map { tuple =>
        tuple._1 -> tuple._2.toVector
      }

    // selfish assignment is assume to be the first alternative for each request
    val selfishAssignmentSelection: List[Int] = alts.toList.map { _ =>
      0
    }

    // evaluate selfish assignment cost
    val selfishAssignmentCostF: F[Cost] =
      SelectionAlgorithm.evaluateCostOfSelection(
        alts.values.flatMap { _.headOption }.toList,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      )

    // run iterative sampling using selfish as the start state
    for {
      selfishCost <- selfishAssignmentCostF
      startState = SelectionState(
        bestSelectionIndices = selfishAssignmentSelection,
        bestCost = selfishCost,
        samples = NaturalNumber.One,
        startTime = startTime
      )
      endState <- RandomSamplingSelectionAlgorithm.performRandomSampling(
        startState,
        indexedAlts.values,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction,
        terminationFunction,
        random)

    } yield {

      // re-cast as Response objects
      val responses: List[Response] =
        indexedAlts
          .zip(endState.bestSelectionIndices)
          .map {
            case ((request, alts), idx) =>
              Response(request, alts(idx).map { _.edgeId })
          }
          .toList

      // final return value
      SelectionAlgorithm.Result(
        responses,
        endState.bestCost,
        endState.samples
      )
    }
  }
}



object RandomSamplingSelectionAlgorithm {

  /**
    * given some sampling state, perform a single random sampling/evaluation, and pass along the best-seen sampling result
    * @param state up to this point, the best seen selection and it's cost, tracking our search state
    * @param indexedAlts all alternatives as an indexed structure
    * @param roadNetwork
    * @param pathToMarginalFlowsFunction
    * @param combineFlowsFunction
    * @param marginalCostFunction
    * @param terminationFunction
    * @param random
    * @tparam F the context for communications between modules
    * @tparam V vertex attribute type
    * @tparam E edge attribute type
    * @return
    */
  def performRandomSampling[F[_]: Monad, V, E](
      state                      : SelectionAlgorithm.SelectionState,
      indexedAlts                : Iterable[Vector[Path]],
      roadNetwork                : RoadNetwork[F, V, E],
      pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
      combineFlowsFunction       : Iterable[Flow] => Flow,
      marginalCostFunction       : E => Flow => Cost,
      terminationFunction        : SelectionState => Boolean,
      random                     : Random
  ): F[SelectionAlgorithm.SelectionState] = {

    // encase loop in F[_]-friendly recursion
    state.iterateUntilM { state =>

      // randomly pick indices and extract the associated paths
      val (thisRandomSelectionIndices: List[Int], thisRandomSelectionPaths: List[Path]) =
        indexedAlts
          .map { alts =>
            val altSelectionIdx: Int = random.nextInt(alts.length)
            (altSelectionIdx, alts(altSelectionIdx))
          }
          .toList
          .unzip

      // get cost of this selection
      val costF: F[Cost] = SelectionAlgorithm.evaluateCostOfSelection(
        thisRandomSelectionPaths,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      )

      // recurse with the best-seen selection
      for {
        thisCost <- costF
      } yield {
        if (state.bestCost < thisCost) {
          state.copy(samples = state.samples + NaturalNumber.One)
        } else {
          SelectionAlgorithm.SelectionState(
            bestSelectionIndices = thisRandomSelectionIndices,
            bestCost = thisCost,
            samples = state.samples + NaturalNumber.One,
            state.startTime
          )
        }
      }

    }(terminationFunction) // terminate with user state evaluation
  }
}
