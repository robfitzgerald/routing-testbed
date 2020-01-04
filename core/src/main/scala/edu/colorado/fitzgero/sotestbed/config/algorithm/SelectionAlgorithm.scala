package edu.colorado.fitzgero.sotestbed.config.algorithm

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.selection
import edu.colorado.fitzgero.sotestbed.algorithm.selection.RandomSamplingSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts.LocalMCTSSelectionAlgorithm

sealed trait SelectionAlgorithm {
  def build[F[_]: Monad, V, E](): selection.SelectionAlgorithm[F, V, E]
  def selectionTerminationFunction: SelectionTerminationFunction,
}
object SelectionAlgorithm {
  final case class RandomSamplingSelection(
    seed: Long,
    selectionTerminationFunction: SelectionTerminationFunction,
  ) extends SelectionAlgorithm {
    override def build[F[_]: Monad, V, E](): selection.SelectionAlgorithm[F, V, E] = {
      new RandomSamplingSelectionAlgorithm(seed)
    }
  }
  final case class LocalMCTSSelection(
    seed: Long,
    selectionTerminationFunction: SelectionTerminationFunction,
  ) extends SelectionAlgorithm {
    override def build[F[_] : Monad, V, E](): selection.SelectionAlgorithm[F, V, E] = {
      // todo:
      //  the LocalMCTS selection does mutable ops that need to run in an IO; just calling F a Monad here
      //  doesn't tell us that the F is a typeclass with unsafeRunSync or anything like that.
      // it's an unfortunate design choice. perhaps we change up the effect context here somehow? or,
      // could the operations it depends on drop the effect typeclass?
      ???
    }
  }
}