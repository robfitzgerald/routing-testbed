package edu.colorado.fitzgero.sotestbed.config

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm

sealed trait SelectionAcceptanceFunctionConfig {
  def build(): SelectionAlgorithm.Result => Boolean
}

object SelectionAcceptanceFunctionConfig {

  final case object AcceptAll extends SelectionAcceptanceFunctionConfig {

    def build(): SelectionAlgorithm.Result => Boolean = (_: SelectionAlgorithm.Result) => true
  }

  final case class ProbabilisticAcceptance(probability: Double, seed: Long) extends SelectionAcceptanceFunctionConfig {

    require(0.0 <= probability, "probability must be in the range [0, 1]")
    require(probability <= 1.0, "probability must be in the range [0, 1]")

    def build(): SelectionAlgorithm.Result => Boolean = {
      val random: Random = new Random(seed)
      (_: SelectionAlgorithm.Result) =>
        {
          random.nextDouble < probability
        }
    }
  }

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
