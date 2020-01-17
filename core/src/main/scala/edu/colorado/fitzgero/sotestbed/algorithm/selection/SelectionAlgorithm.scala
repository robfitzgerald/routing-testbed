package edu.colorado.fitzgero.sotestbed.algorithm.selection

import scala.annotation.tailrec
import scala.util.Try

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost._
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._
import edu.colorado.fitzgero.sotestbed.util.MultiSetIterator

abstract class SelectionAlgorithm[F[_]: Monad, V, E] {

  // invariant: first path for each agent should be the shortest path
  // invariant: at least one path exists for each request

  def selectRoutes(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): F[SelectionAlgorithm.Result]
}

// hey, here's a few ideas for a SelectionAlgorithm:
//   - the number of agents per selection algorithm is capped, but, multiples are run simultaneously, and the "best" solutions are resolved afterward
//   - groupings by overlap/visitation to common geo-cells

object SelectionAlgorithm {
  final case class Result(
    selectedRoutes: List[Response] = List.empty,
    estimatedCost: Cost = Cost.Zero,
    samples: NonNegativeNumber = NonNegativeNumber.Zero
  )

  final case class SelectionState(
    bestSelectionIndices: Seq[Int],
    bestOverallCost: Cost,
    agentPathCosts: List[Cost],
    samples: NonNegativeNumber,
    startTime: Long
  )

  final case class SelectionCost(
    overallCost: Cost = Cost.Zero,
    agentPathCosts: List[Cost] = List.empty
  )

  /**
    * evaluates the cost of this selection
    * @param paths a selection of path alternatives
    * @param roadNetwork the current road network conditions
    * @param pathToMarginalFlowsFunction
    * @param combineFlowsFunction
    * @param marginalCostFunction
    * @tparam F
    * @tparam V
    * @tparam E
    * @return
    */
  def evaluateCostOfSelection[F[_]: Monad, V, E](
    paths: List[Path],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost,
  ): F[SelectionCost] = {
    for {
      edgesMap <- roadNetwork.edges(paths.flatten.map { _.edgeId }).map {
        _.map { tup =>
          (tup.edgeId, tup.attribute)
        }.toMap
      }
      costs <- paths
        .traverse(path => pathToMarginalFlowsFunction(roadNetwork, path))
        .map {
          _.flatten
            .groupBy { case (edgeId, _) => edgeId }
            .flatMap {
              case (edgeId, edgesAndFlows) =>
                edgesMap
                  .get(edgeId)
                  .map { edge: E =>
                    val flow: Flow = combineFlowsFunction(
                      edgesAndFlows
                        .map {
                          case (_, flow) =>
                            flow
                        }
                    )
                    val cost: Cost = marginalCostFunction(edge)(flow)
                    (edgeId, cost)
                  }
            }
            .toMap
        }
    } yield {
      if (costs.isEmpty) {
        SelectionCost()
      } else {
        val pathCosts: List[Cost] = for {
          path <- paths
        } yield {
          if (path.isEmpty) Cost.Zero
          else
            path.map { seg =>
              costs(seg.edgeId)
            }.sum
        }

        SelectionCost(
          costs.values.sum,
          pathCosts
        )
      }
    }
  }

  /**
    * tests to see if the total number of possible combinations is less than
    * a threshold, making this problem appropriate for an exhaustive search
    * @param paths a selection problem input
    * @param threshold the max number of samples allowed for exhaustive search
    * @return
    */
  def numCombinationsLessThanThreshold(
    paths: Map[Request, List[Path]],
    threshold: Int
  ): Boolean = {
    @tailrec
    def _combinations(pathsPerAgent: List[Int], count: Int = 1): Boolean = {
      if (pathsPerAgent.isEmpty) true
      else if (count * pathsPerAgent.head > threshold) false
      else _combinations(pathsPerAgent.tail, count * pathsPerAgent.head)
    }
    _combinations(paths.map{ case(_, paths) => paths.length }.toList)
  }

  /**
    * performs an exhaustive search for the best combination
    * @param paths
    * @param roadNetwork
    * @param pathToMarginalFlowsFunction
    * @param combineFlowsFunction
    * @param marginalCostFunction
    * @tparam F
    * @tparam V
    * @tparam E
    * @return
    */
  def performExhaustiveSearch[F[_]: Monad, V, E](
    paths: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): F[Result] = {
    if (paths.isEmpty) Monad[F].pure { Result() }
    else {
      val startTime: Long = System.currentTimeMillis

      // rewrap as a multiset, set up an iterator
      val asMultiSet: Array[Array[List[PathSegment]]] =
        paths.map{ case (_, paths) => paths.toArray}.toArray
      val iterator: MultiSetIterator[List[PathSegment]] = MultiSetIterator(asMultiSet)

      // initialize with the selfish routing selection (wrapped in call to RoadNetwork effect)
      val selfishIndices: Array[Int] = Array.fill{asMultiSet.length}(0)
      val selfishPaths: List[Path] = iterator.next().toList
      evaluateCostOfSelection(
        selfishPaths,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      ) map { selfishCost: SelectionCost =>

        val initialSelfishSelection: SelectionState = SelectionState(
          bestSelectionIndices = selfishIndices,
          selfishCost.overallCost,
          selfishCost.agentPathCosts,
          NonNegativeNumber.One,
          startTime
        )

        // iterate through all combinations
        val finalStateF: F[SelectionState] = initialSelfishSelection.iterateUntilM{ state: SelectionState =>
          val thisPaths: List[Path] = iterator.next().toList
          val thisIndices: Array[Int] = iterator.pos
          evaluateCostOfSelection(
            thisPaths,
            roadNetwork,
            pathToMarginalFlowsFunction,
            combineFlowsFunction,
            marginalCostFunction
          ).map { thisCost =>
            if (state.bestOverallCost < thisCost.overallCost) {
              state.copy(samples = state.samples + NonNegativeNumber.One)
            } else {
              state.copy(
                bestSelectionIndices = thisIndices,
                bestOverallCost = thisCost.overallCost,
                agentPathCosts = thisCost.agentPathCosts,
                samples = state.samples + NonNegativeNumber.One,
              )
            }
          }
        }{_ => !iterator.hasNext}

        // package the responses associated with the best selection
        finalStateF.map{ finalState =>
          val responses: List[Response] =
            paths
              .zip(finalState.bestSelectionIndices)
              .zip(finalState.agentPathCosts)
              .map {
                case (((request, alts), idx), cost) =>
                  Response(request, alts(idx).map { _.edgeId }, cost)
              }
              .toList

          Result(
            selectedRoutes = responses,
            estimatedCost = finalState.bestOverallCost,
            samples = finalState.samples
          )
        }
      }
    }.flatten
  }
}
