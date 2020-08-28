package edu.colorado.fitzgero.sotestbed.config

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm

sealed trait SelectionAcceptanceFunctionConfig {
  def build(): SelectionAlgorithm.Result => Boolean
}

object SelectionAcceptanceFunctionConfig {
  final case object DismissCompleteSearches extends SelectionAcceptanceFunctionConfig {

    def build(): SelectionAlgorithm.Result => Boolean =
      (result: SelectionAlgorithm.Result) => {
        // accepts only results which did not see 100% of the search space
        result.ratioOfSearchSpaceExplored < 1.0
      }
  }

  final case object DismissIncompleteSearches extends SelectionAcceptanceFunctionConfig {

    def build(): SelectionAlgorithm.Result => Boolean =
      (result: SelectionAlgorithm.Result) => {
        // accepts only results which saw exactly 100% of the search space
        // (or, for sampling-based methods, that saw at least 100% of the search space)
        result.ratioOfSearchSpaceExplored >= 1.0
      }
  }
}
