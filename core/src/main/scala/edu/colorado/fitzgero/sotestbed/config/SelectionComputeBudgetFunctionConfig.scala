package edu.colorado.fitzgero.sotestbed.config

sealed trait SelectionComputeBudgetFunctionConfig

object SelectionComputeBudgetFunctionConfig {

  final case class DurationMs(milliseconds: Long) extends SelectionComputeBudgetFunctionConfig

  final case class PercentExplored(explored: Double) extends SelectionComputeBudgetFunctionConfig

  final case class Samples(maxIterations: Int) extends SelectionComputeBudgetFunctionConfig

  final case class Combined(configs: List[SelectionComputeBudgetFunctionConfig])
      extends SelectionComputeBudgetFunctionConfig

  final case class DurationMsAndPercentExplored(milliseconds: Long, explored: Double)
      extends SelectionComputeBudgetFunctionConfig

}
