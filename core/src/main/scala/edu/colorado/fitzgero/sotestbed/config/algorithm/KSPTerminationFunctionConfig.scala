package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm.AltPathsState

sealed trait KSPTerminationFunctionConfig {
  def build(): AltPathsState => Boolean
}

object KSPTerminationFunctionConfig {

  /**
    * short-circuits the ksp search when ${seen} paths have been reviewed
    * @param seen the max number of paths to "see"
    */
  final case class PathsSeen(
    seen: Int
  ) extends KSPTerminationFunctionConfig {

    def build(): AltPathsState => Boolean = (state: AltPathsState) => state.alts.length >= seen
  }

  /**
    * runs until entire search frontier has been explored
    */
  final case object UntilKFoundOrEmptyFrontier extends KSPTerminationFunctionConfig {

    def build(): AltPathsState => Boolean = (_: AltPathsState) => false
  }

}
