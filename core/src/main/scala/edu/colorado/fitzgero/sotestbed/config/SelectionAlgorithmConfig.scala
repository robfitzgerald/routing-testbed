package edu.colorado.fitzgero.sotestbed.config

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.algorithm.selection
import edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts.{
  DefaultPolicy,
  ExpandPolicy,
  MCTS2SelectionAlgorithm,
  RunnerType
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{
  AgentOrdering,
  PathOrdering,
  SelectionAlgorithm,
  TrueShortestSelectionAlgorithm
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.random.{
  Rand2SelectionAlgorithm,
  RandomSamplingSelectionAlgorithm
}
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate

sealed trait SelectionAlgorithmConfig {
  def build(): selection.SelectionAlgorithm[IO, Coordinate, EdgeBPR]
}

object SelectionAlgorithmConfig {

  final case class RandomSamplingSelection(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    selectionTerminationFunction: SelectionTerminationFunctionConfig,
    selectionAcceptanceFunction: SelectionAcceptanceFunctionConfig
  ) extends SelectionAlgorithmConfig {

    def build(): selection.SelectionAlgorithm[IO, Coordinate, EdgeBPR] = {
      new RandomSamplingSelectionAlgorithm[IO, Coordinate, EdgeBPR](
        seed,
        exhaustiveSearchSampleLimit,
        selectionTerminationFunction.build(),
        selectionAcceptanceFunction.build()
      )
    }
  }

  final case class RandomSelection2(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
    computeBudgetTestRate: Int
  ) extends SelectionAlgorithmConfig {

    def build(): SelectionAlgorithm[IO, Coordinate, EdgeBPR] = {
      new Rand2SelectionAlgorithm(seed, exhaustiveSearchSampleLimit, computeBudgetFunctionConfig, computeBudgetTestRate)
    }
  }

  final case class LocalMCTSSelection(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    minimumAverageBatchTravelImprovement: Cost,
    selectionTerminationFunction: SelectionTerminationFunctionConfig
  ) extends SelectionAlgorithmConfig {
    assert(
      minimumAverageBatchTravelImprovement.value >= 0,
      f"local-mcts-selection.minimum-average-batch-travel-improvement must be non-negative but found ${minimumAverageBatchTravelImprovement.value}%.2f"
    )

    def build(): selection.SelectionAlgorithm[IO, Coordinate, EdgeBPR] = {
      new selection.mcts.LocalMCTSSelectionAlgorithm[Coordinate, EdgeBPR](
        seed,
        exhaustiveSearchSampleLimit,
        minimumAverageBatchTravelImprovement,
        selectionTerminationFunction.build()
      )
    }
  }

  final case class LocalMCTS2(
    seed: Option[Long],
    exhaustiveSearchSampleLimit: Int,
    computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
    expandPolicy: ExpandPolicy,
    agentOrdering: Option[AgentOrdering],
    pathOrdering: Option[PathOrdering],
    computeBudgetTestRate: Int
  ) extends SelectionAlgorithmConfig {

    def build(): SelectionAlgorithm[IO, Coordinate, EdgeBPR] = {
      new MCTS2SelectionAlgorithm(
        DefaultPolicy.UniformRandomPolicy(seed),
        expandPolicy,
        agentOrdering,
        pathOrdering,
        RunnerType.Default,
        seed.getOrElse(System.currentTimeMillis),
        exhaustiveSearchSampleLimit,
        computeBudgetFunctionConfig,
        computeBudgetTestRate
      )
    }
  }

  final case object TspSelection extends SelectionAlgorithmConfig {

    def build(): selection.SelectionAlgorithm[IO, Coordinate, EdgeBPR] =
      TrueShortestSelectionAlgorithm.apply[Coordinate, EdgeBPR]()
  }
}
