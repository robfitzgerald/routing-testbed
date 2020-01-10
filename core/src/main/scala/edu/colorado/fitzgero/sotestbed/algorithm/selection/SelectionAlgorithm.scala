package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.Monad
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost._
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

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
    selectedRoutes: List[Response],
    estimatedCost: Cost,
    samples: NonNegativeNumber
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
}
