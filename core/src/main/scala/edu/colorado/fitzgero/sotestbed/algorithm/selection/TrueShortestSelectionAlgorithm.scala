package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

/**
  * simply returns the true shortest paths for each agent
  */
class TrueShortestSelectionAlgorithm extends SelectionAlgorithm {

  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    marginalCostFunction: EdgeBPR => Flow => Cost
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
        selfishCost = totalCost,
        updatedBank = bank
      )

      IO { result }
    }
  }
}

object TrueShortestSelectionAlgorithm {
  def apply[V, E](): TrueShortestSelectionAlgorithm = new TrueShortestSelectionAlgorithm
}
