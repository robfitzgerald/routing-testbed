package edu.colorado.fitzgero.sotestbed.config

import java.io.File
import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import edu.colorado.fitzgero.sotestbed.algorithm.selection
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.{
  AuctionPolicy,
  CongestionObservationType,
  DriverPolicy,
  KarmaSelectionAlgorithm,
  NetworkPolicyConfig
}
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
import edu.colorado.fitzgero.sotestbed.algorithm.selection.rl.{Env, RLSelectionAlgorithm, Space, SpaceV1Ops}
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.rllib.{Grouping, PolicyClientOps}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.AllocationTransform

sealed trait SelectionAlgorithmConfig {
  def build(outDir: Path): selection.SelectionAlgorithm
}

object SelectionAlgorithmConfig {

  final case class RandomSamplingSelection(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    selectionTerminationFunction: SelectionTerminationFunctionConfig,
    selectionAcceptanceFunction: SelectionAcceptanceFunctionConfig
  ) extends SelectionAlgorithmConfig {

    def build(outDir: Path): selection.SelectionAlgorithm = {
      new RandomSamplingSelectionAlgorithm(
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

    def build(outDir: Path): SelectionAlgorithm = {
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

    def build(outDir: Path): selection.SelectionAlgorithm = {
      new selection.mcts.LocalMCTSSelectionAlgorithm(
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
    mctsCoefficient: Option[Double],
    computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
    expandPolicy: ExpandPolicy,
    agentOrdering: Option[AgentOrdering],
    pathOrdering: Option[PathOrdering],
    computeBudgetTestRate: Int
  ) extends SelectionAlgorithmConfig {

    def build(outDir: Path): SelectionAlgorithm = {
      new MCTS2SelectionAlgorithm(
        DefaultPolicy.UniformRandomPolicy(seed),
        expandPolicy,
        agentOrdering,
        pathOrdering,
        RunnerType.Default,
        mctsCoefficient,
        seed.getOrElse(System.currentTimeMillis),
        exhaustiveSearchSampleLimit,
        computeBudgetFunctionConfig,
        computeBudgetTestRate
      )
    }
  }

  final case class RLSelection(
    host: String,
    port: Int,
    space: Space,
    groupingFile: File
  ) extends SelectionAlgorithmConfig {

    def build(outDir: Path): SelectionAlgorithm = {
      val algEffect = for {
        grouping <- Grouping(groupingFile)
        alg <- RLSelectionAlgorithm(
          host = host,
          port = port,
          env = Env.MultiAgentGroupedEnvironment(space, grouping)
        )
      } yield alg

      // in the future, return IO[SelectionAlgorithm]
      algEffect.unsafeRunSync
    }
  }

  final case class KarmaSelection(
    driverPolicy: DriverPolicyConfig,
    networkPolicy: NetworkPolicyConfig,
    auctionPolicy: AuctionPolicy,
    congestionObservation: CongestionObservationType,
    freeFlowCostFunction: FreeFlowCostFunctionConfig,
    marginalCostFunction: MarginalCostFunctionConfig,
    bankConfig: BankConfig,
    allocationTransform: AllocationTransform,
    seed: Option[Long]
  ) extends SelectionAlgorithmConfig {

    def build(outDir: Path): SelectionAlgorithm = {
      val mcf = marginalCostFunction.build()
      driverPolicy.buildDriverPolicy match {
        case Left(value) => throw value
        case Right(dp) =>
          KarmaSelectionAlgorithm(
            dp,
            networkPolicy,
            auctionPolicy,
            congestionObservation,
            bankConfig,
            freeFlowCostFunction,
            mcf,
            seed,
            outDir,
            allocationTransform
          )
      }
    }
  }

  final case object TspSelection extends SelectionAlgorithmConfig {

    def build(outDir: Path): selection.SelectionAlgorithm =
      TrueShortestSelectionAlgorithm.apply[Coordinate, EdgeBPR]()
  }
}
