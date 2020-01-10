package edu.colorado.fitzgero.sotestbed.algorithm.selection

import scala.util.Random

import cats.Monad
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.{SelectionCost, SelectionState}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

class RandomSamplingSelectionAlgorithm[F[_]: Monad, V, E](
    seed: Long,
    terminationFunction: SelectionState => Boolean
) extends SelectionAlgorithm[F, V, E] with LazyLogging {

  import SelectionAlgorithm._

  val random: Random = new Random(seed)

  case class EdgeAndMarginalFlow(edgeId: EdgeId, flow: Flow)

  def selectRoutes(
      alts: Map[Request, List[Path]],
      roadNetwork: RoadNetwork[F, V, E],
      pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      marginalCostFunction: E => Flow => Cost
  ): F[SelectionAlgorithm.Result] = {

    logger.debug(s"selectRoutes called with ${alts.size} requests")

    val startTime: Long = System.currentTimeMillis

    if (alts.isEmpty) {
      Monad[F].pure {
        SelectionAlgorithm.Result(
          List.empty,
          Cost.Zero,
          NonNegativeNumber.Zero
        )
      }
    } else {
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
        selfishCost = selfishCostPayload.overallCost
        startState = SelectionState(
          bestSelectionIndices = selfishAssignmentSelection,
          bestOverallCost = selfishCost,
          agentPathCosts = selfishCostPayload.agentPathCosts,
          samples = NonNegativeNumber.One,
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
            .zip(endState.agentPathCosts)
            .map {
              case (((request, alts), idx), cost) =>
                Response(request, alts(idx).map { _.edgeId }, cost)
            }
            .toList
//        val bestCostStr: String = endState.bestOverallCost.toString.padTo(10, ' ')
        logger.info(s"AGENTS: ${responses.length} SAMPLES: ${endState.samples}")
        logger.info(s"COST_EST: BEST ${endState.bestOverallCost}, SELFISH $selfishCost, DIFF ${selfishCost - endState.bestOverallCost}")

        // final return value
        SelectionAlgorithm.Result(
          responses,
          endState.bestOverallCost,
          endState.samples
        )
      }
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

    if (indexedAlts.size <= 1) {
      Monad[F].pure{
        state
      }
    } else {
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
          if (state.bestOverallCost < thisCost) {
            state.copy(samples = state.samples + NonNegativeNumber.One)
          } else {
            SelectionAlgorithm.SelectionState(
              bestSelectionIndices = thisRandomSelectionIndices,
              bestOverallCost = thisCost,
              agentPathCosts = thisCostPayload.agentPathCosts,
              samples = state.samples + NonNegativeNumber.One,
              state.startTime
            )
          }
        }

      }(terminationFunction) // terminate with user state evaluation
    }
  }
}
