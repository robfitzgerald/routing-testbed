package edu.colorado.fitzgero.sotestbed.algorithm.selection

import scala.annotation.tailrec

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

  /**
    * result of applying a selection algorithm
    *
    * @param selectedRoutes
    * @param estimatedCost
    * @param selfishCost
    * @param travelTimeDiff
    * @param averageTravelTimeDiff
    * @param samples
    */
  final case class Result(
    selectedRoutes: List[Response] = List.empty,
    estimatedCost: Cost = Cost.Zero,
    selfishCost: Cost = Cost.Zero,
    travelTimeDiff: Cost = Cost.Zero,
    averageTravelTimeDiff: Cost = Cost.Zero,
    samples: NonNegativeNumber = NonNegativeNumber.Zero,
    ratioOfSearchSpaceExplored: Double = 0.0
  )

  final case class SelectionState(
    bestSelectionIndices: Seq[Int],
    bestOverallCost: Cost,
    agentPathCosts: List[Cost],
    samples: NonNegativeNumber,
    searchSpaceSize: BigDecimal,
    startTime: Long
  ) {

    /**
      * # samples proportional to search space, bounded by [0, 1]
      * @return ratio of search space explored. may not be exact in the case of random sampling
      */
    def ratioOfSearchSpaceExplored: Double = {
      if (searchSpaceSize == BigDecimal(0)) {
        0.0
      } else {
        val ratio: Double        = (BigDecimal(samples.value) / searchSpaceSize).toDouble
        val boundedRatio: Double = math.max(math.min(ratio, 1.0), 0.0)
        boundedRatio

      }
    }
  }

  final case class SelectionCost(
    overallCost: Cost = Cost.Zero,
    agentPathCosts: List[Cost] = List.empty
  )

  /**
    * evaluates the cost of this selection using the user-provided path and cost functions
    *
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
    // set up the list of edges to account for, and a lookup table of edge attributes
    val allSelectedEdgeIds: List[EdgeId] = paths.flatten.map { _.edgeId } // at this point, not yet distinct! we need to count each occurrence still
    val edgeIdsWithEdgeValues: F[Map[EdgeId, E]] = roadNetwork.edges(allSelectedEdgeIds).map {
      _.map { tup =>
        (tup.edgeId, tup.attribute)
      }.toMap
    }

    // use the provided function to convert the provided paths to their flow contributions
    val flowsByEdgeId: F[List[(EdgeId, Flow)]] = paths.traverse(path => pathToMarginalFlowsFunction(roadNetwork, path)).map { _.flatten }

    val result: F[SelectionCost] = for {
      edgesMap <- edgeIdsWithEdgeValues
      costs <- flowsByEdgeId.map { fs =>
        marginalCostForEdgesWithFlows(fs, edgesMap, combineFlowsFunction, marginalCostFunction)
      }
    } yield {
      if (costs.isEmpty) {
        SelectionCost()
      } else {

        // assign the marginal cost to each edge in each path
        val pathCosts: List[Cost] = paths
          .foldLeft(List.empty[Cost]) { (acc, path) =>
            val pathCost = path.foldLeft(Cost.Zero) { (acc, seg) =>
              acc + costs.getOrElse(seg.edgeId, Cost.Zero)
            }
            pathCost +: acc
          }
          .reverse

        val overallCost = pathCosts.sum

        SelectionCost(
          overallCost,
          pathCosts
        )
      }
    }

    result
  }

  /**
    * computes the marginal cost contribution for each edge
    *
    * @param edgeIdsWithFlows
    * @param edgeLookup
    * @param combineFlowsFunction
    * @param marginalCostFunction
    * @tparam E
    * @return
    */
  def marginalCostForEdgesWithFlows[E](
    edgeIdsWithFlows: List[(EdgeId, Flow)],
    edgeLookup: Map[EdgeId, E],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): Map[EdgeId, Cost] = {

    val result = for {
      (edgeId, edgesAndFlows) <- edgeIdsWithFlows.groupBy { case (edgeId, _) => edgeId }
      edge                    <- edgeLookup.get(edgeId)
    } yield {
      val flowsForThisEdge    = edgesAndFlows.map { case (_, flow) => flow }
      val combinedFlows: Flow = combineFlowsFunction(flowsForThisEdge)

      // let's return the cost difference between the current link cost and the cost when adding these flows
      val unloadedCost: Cost = marginalCostFunction(edge)(Flow.Zero)
      val loadedCost: Cost   = marginalCostFunction(edge)(combinedFlows)
      val marginalCost: Cost = loadedCost - unloadedCost

      (edgeId, marginalCost)
    }

    result
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
    _combinations(paths.map { case (_, paths) => paths.length }.toList)
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
    * @return the best selection and the cost of the true shortest paths combination
    */
  def performExhaustiveSearch[F[_]: Monad, V, E](
    paths: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[F, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[F, V, E], Path) => F[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): F[(Result, Cost)] = {
    if (paths.isEmpty) Monad[F].pure { (Result(), Cost.Zero) } else {
      val startTime: Long = System.currentTimeMillis

      // rewrap as a multiset, set up an iterator
      val asMultiSet: Array[Array[List[PathSegment]]] =
        paths.map { case (_, paths) => paths.toArray }.toArray
      val iterator: MultiSetIterator[List[PathSegment]] = MultiSetIterator(asMultiSet)

      // initialize with the selfish routing selection (wrapped in call to RoadNetwork effect)
      val selfishIndices: Array[Int]  = Array.fill { asMultiSet.length }(0)
      val selfishPaths: List[Path]    = iterator.next().toList
      val searchSpaceSize: BigDecimal = BigDecimal(paths.values.map { _.size }.product)
      evaluateCostOfSelection(
        selfishPaths,
        roadNetwork,
        pathToMarginalFlowsFunction,
        combineFlowsFunction,
        marginalCostFunction
      ).map { selfishCost: SelectionCost =>
        val initialSelfishSelection: SelectionState = SelectionState(
          bestSelectionIndices = selfishIndices,
          selfishCost.overallCost,
          selfishCost.agentPathCosts,
          NonNegativeNumber.One,
          searchSpaceSize = searchSpaceSize,
          startTime
        )

        // iterate through all combinations
        val finalStateF: F[SelectionState] = initialSelfishSelection.iterateUntilM { state: SelectionState =>
          val thisPaths: List[Path]   = iterator.next().toList
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
        } { _ =>
          !iterator.hasNext
        }

        // package the responses associated with the best selection
        finalStateF.map { finalState =>
          val responses: List[Response] =
            paths
              .zip(finalState.bestSelectionIndices)
              .zip(finalState.agentPathCosts)
              .map {
                case (((request, alts), idx), cost) =>
                  Response(request, idx, alts(idx).map { _.edgeId }, cost)
              }
              .toList

          val improvement: Cost        = Cost(selfishCost.overallCost - finalState.bestOverallCost)
          val averageImprovement: Cost = Cost((selfishCost.overallCost - finalState.bestOverallCost).value / paths.size)

          val searchResult: Result = Result(
            selectedRoutes = responses,
            estimatedCost = finalState.bestOverallCost,
            selfishCost = selfishCost.overallCost,
            travelTimeDiff = improvement,
            averageTravelTimeDiff = averageImprovement,
            samples = finalState.samples
          )

          (searchResult, selfishCost.overallCost)
        }
      }
    }.flatten
  }
}
