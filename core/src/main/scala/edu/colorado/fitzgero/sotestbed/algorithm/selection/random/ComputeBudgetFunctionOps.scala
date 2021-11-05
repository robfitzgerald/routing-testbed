package edu.colorado.fitzgero.sotestbed.algorithm.selection.random

import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import edu.colorado.fitzgero.sotestbed.config.SelectionComputeBudgetFunctionConfig

object ComputeBudgetFunctionOps {

  /**
    * interpreter for compute budget function configurations in a Rand2 context
    * @param conf
    */
  implicit class ComputeBudgetFunctionOpsInstance(conf: SelectionComputeBudgetFunctionConfig) {

    /**
      * instantiate the compute budget function
      * @param searchSpaceSize the search space size for this problem instance
      * @return function that is true if we can compute
      */
    def makeComputeBudgetFn(searchSpaceSize: BigDecimal): (Int, Long) => Boolean = {

      conf match {
        case SelectionComputeBudgetFunctionConfig.DurationMs(milliseconds) =>
          (_: Int, startTime: Long) => {
            val result = System.currentTimeMillis < startTime + milliseconds
            result
          }
        case SelectionComputeBudgetFunctionConfig.PercentExplored(explored) =>
          (iterations: Int, _: Long) => {
            val result = (BigDecimal(iterations) / searchSpaceSize).toDouble <= explored
            result
          }
        case SelectionComputeBudgetFunctionConfig.Samples(maxIterations) =>
          (iterations: Int, _: Long) => {
            val result = iterations <= maxIterations
            result
          }
        case SelectionComputeBudgetFunctionConfig.DurationMsAndPercentExplored(ms, explored) =>
          (iterations: Int, startTime: Long) => {
            val timeBudget = System.currentTimeMillis < startTime + ms
            val iterBudget = (BigDecimal(iterations) / searchSpaceSize).toDouble <= explored
            timeBudget && iterBudget
          }
        case SelectionComputeBudgetFunctionConfig.Combined(xs) =>
          val fns: List[(Int, Long) => Boolean] = xs.map { c => c.makeComputeBudgetFn(searchSpaceSize) }
          (i: Int, t: Long) => fns.forall { fn => fn(i, t) }
      }
    }
  }
}
