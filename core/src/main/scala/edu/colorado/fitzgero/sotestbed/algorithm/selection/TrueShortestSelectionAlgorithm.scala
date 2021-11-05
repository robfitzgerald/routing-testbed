package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  * simply returns the true shortest paths for each agent
  */
class TrueShortestSelectionAlgorithm[V, E] extends SelectionAlgorithm[IO, V, E] {

  def selectRoutes(
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, V, E],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, V, E], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: E => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {
    if (alts.isEmpty) {
      IO { SelectionAlgorithm.SelectionAlgorithmResult() }
    } else {
      val responses: List[Response] = {
        for {
          (request, alts) <- alts
        } yield {
          val trueShortestCost: SelectionCost = SelectionAlgorithm
            .evaluateCostOfSelection(
              List(alts.head),
              roadNetwork,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            )
            .unsafeRunSync()
          Response(request, pathIndex = 0, alts.head.map { _.edgeId }, trueShortestCost.overallCost)
        }
      }.toList

      val totalCost: Cost = responses.map { _.costEstimate }.reduce { _ + _ }
      val result = SelectionAlgorithm.SelectionAlgorithmResult(
        selectedRoutes = responses,
        estimatedCost = totalCost,
        selfishCost = totalCost
      )

      IO { result }
    }
  }
}

object TrueShortestSelectionAlgorithm {
  def apply[V, E](): TrueShortestSelectionAlgorithm[V, E] = new TrueShortestSelectionAlgorithm
}
