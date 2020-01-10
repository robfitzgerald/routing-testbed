package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm.AltPathsState

sealed trait KSPTerminationFunctionConfig {
  def build(): AltPathsState => Boolean
}
object KSPTerminationFunctionConfig {
  final case class PathsSeen(
    seen: Int
  ) extends KSPTerminationFunctionConfig {
    def build(): AltPathsState => Boolean =
      (state: AltPathsState) =>
        state.alts.length >= seen
  }
}
