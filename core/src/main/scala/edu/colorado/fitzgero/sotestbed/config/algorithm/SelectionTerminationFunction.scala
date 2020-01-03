package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionState

sealed trait SelectionTerminationFunction {
  def build(): SelectionState => Boolean
}
object SelectionTerminationFunction {
  final case class ComputeBudget(
    durationMS: Int
  ) extends SelectionTerminationFunction {
    def build(): SelectionState => Boolean =
      (state: SelectionState) =>
        state.startTime + durationMS < System.currentTimeMillis
  }
}
