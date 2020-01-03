package edu.colorado.fitzgero.sotestbed.config.algorithm

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.selection
import edu.colorado.fitzgero.sotestbed.algorithm.selection.RandomSamplingSelectionAlgorithm

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
}