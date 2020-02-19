package edu.colorado.fitzgero.sotestbed.config.algorithm

import cats.Monad
import cats.effect.SyncIO

import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.selection

sealed trait SelectionAlgorithmConfig {
  def selectionTerminationFunction: SelectionTerminationFunctionConfig
}
object SelectionAlgorithmConfig {

  final case class RandomSamplingSelection(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    selectionTerminationFunction: SelectionTerminationFunctionConfig,
  ) extends SelectionAlgorithmConfig {
    def build[F[_]: Monad, V, E](): selection.SelectionAlgorithm[F, V, E] = {
      new selection.RandomSamplingSelectionAlgorithm[F, V, E](
        seed,
        exhaustiveSearchSampleLimit,
        selectionTerminationFunction.build()
      )
    }
  }
  final case class LocalMCTSSelection(
    seed: Long,
    exhaustiveSearchSampleLimit: Int,
    selectionTerminationFunction: SelectionTerminationFunctionConfig,
  ) extends SelectionAlgorithmConfig {
    def build[V, E](): selection.SelectionAlgorithm[SyncIO, V, E] = {
      // todo:
      //  the LocalMCTS selection does mutable ops that need to run in an IO; just calling F a Monad here
      //  doesn't tell us that the F is a typeclass with unsafeRunSync or anything like that.
      // it's an unfortunate design choice. perhaps we change up the effect context here somehow? or,
      // could the operations it depends on drop the effect typeclass?
      new selection.mcts.LocalMCTSSelectionAlgorithm[V, E](
        seed,
        exhaustiveSearchSampleLimit,
        selectionTerminationFunction.build()
      )
    }
  }
}