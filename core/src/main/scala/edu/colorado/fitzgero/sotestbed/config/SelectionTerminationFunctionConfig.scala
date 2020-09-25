package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionState

sealed trait SelectionTerminationFunctionConfig {
  // todo: refactor so that a TerminationFunction can be initialized for a batch
  //  with batch-related state such as the batch end time and termination check frequency
  def build(): SelectionState => Boolean
}

object SelectionTerminationFunctionConfig {
  final case class ComputeBudget(
    durationMS: Int
  ) extends SelectionTerminationFunctionConfig {

    def build(): SelectionState => Boolean = { state: SelectionState =>
      {
        val endTime: Long = state.startTime + durationMS
        endTime < System.currentTimeMillis
      }
    }
  }

  /**
    * stop when the search space has been explored to some target coverage ratio,
    * but short-circuit when we have exceeded our compute budget
    *
    * @param targetSearchSpaceExplorationRatio % of the search space to explore
    * @param durationMS cutoff time in milliseconds
    */
  final case class PercentExploredWithComputeBudget(
    targetSearchSpaceExplorationRatio: Double,
    durationMS: Int
  ) extends SelectionTerminationFunctionConfig {

    def build(): SelectionState => Boolean =
      (state: SelectionState) => {
        if (state.searchSpaceSize == BigDecimal(0)) {
          true
        } else {
          val currentCoverage: Double                  = (BigDecimal(state.samples.value) / state.searchSpaceSize).toDouble
          val endTime: Long                            = state.startTime + durationMS
          val exceededSearchExplorationTarget: Boolean = targetSearchSpaceExplorationRatio < currentCoverage
          val exceededComputeBudget: Boolean           = endTime < System.currentTimeMillis
          if (exceededSearchExplorationTarget || exceededComputeBudget) {
            println("")
          }
          exceededSearchExplorationTarget || exceededComputeBudget
        }
      }
  }
}
