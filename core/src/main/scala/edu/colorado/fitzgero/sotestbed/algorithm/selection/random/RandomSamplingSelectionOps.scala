package edu.colorado.fitzgero.sotestbed.algorithm.selection.random

import scala.util.Random

import cats.implicits._
import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

object RandomSamplingSelectionOps {

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
    state: SelectionAlgorithm.SelectionState,
    indexedAlts: Iterable[Vector[Path]],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
    terminationFunction: SelectionState => Boolean,
    random: Random
  ): F[SelectionAlgorithm.SelectionState] = {

    if (indexedAlts.size <= 1) {
      Monad[F].pure {
        state
      }
    } else {
      // encase loop in F[_]-friendly recursion
      state.iterateUntilM { prevState =>
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
        val costF: F[SelectionCost] = SelectionAlgorithm.evaluateCostOfSelection(
          thisRandomSelectionPaths,
          roadNetwork,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )

        // recurse with the best-seen selection
        for {
          thisCostPayload <- costF
          thisCost = thisCostPayload.overallCost
        } yield {
          if (prevState.bestOverallCost < thisCost) {
            // no improvement -> no change
            val nextState: SelectionState = prevState.copy(
              samples = prevState.samples + NonNegativeNumber.One
            )
            nextState
          } else {
            // replace the best selection
            val nextState: SelectionState = prevState.copy(
              bestSelectionIndices = thisRandomSelectionIndices,
              bestOverallCost = thisCost,
              agentPathCosts = thisCostPayload.agentPathCosts,
              samples = prevState.samples + NonNegativeNumber.One
            )
            nextState
          }
        }

      }(terminationFunction) // terminate with user state evaluation
    }
  }
}
