package edu.colorado.fitzgero.sotestbed.config.algorithm

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.KSPAlgorithm.AltPathsState

sealed trait KSPTerminationFunction {
  def build(): AltPathsState => Boolean
}
object KSPTerminationFunction {
  final case class PathsSeen(
    seen: Int
  ) extends KSPTerminationFunction {
    def build(): AltPathsState => Boolean =
      (state: AltPathsState) =>
        state.alts.length >= seen
  }
}
