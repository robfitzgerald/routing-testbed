package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionState

sealed trait SelectionTerminationFunctionConfig {
  def build(): SelectionState => Boolean
}
object SelectionTerminationFunctionConfig {
  final case class ComputeBudget(
    durationMS: Int
  ) extends SelectionTerminationFunctionConfig {
    def build(): SelectionState => Boolean =
      (state: SelectionState) =>
        state.startTime + durationMS < System.currentTimeMillis
  }
}
