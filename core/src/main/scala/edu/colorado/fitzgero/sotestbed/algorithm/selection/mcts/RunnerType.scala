package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import cats.effect.IO
import cats.implicits._

import cse.bdlab.fitzgero.mcts.MCTS2
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceState
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts.ComputeBudgetFunctionOps.ComputeBudgetFunctionOpsInstance
import edu.colorado.fitzgero.sotestbed.config.SelectionComputeBudgetFunctionConfig

sealed trait RunnerType

object RunnerType {

  final case object Default                                extends RunnerType
  final case class EnsembleBootstrap(n: Int, samples: Int) extends RunnerType

  implicit class RunnerTypeOps(runnerType: RunnerType) {

    def run(
      choices: Array[Int],
      evalFn: MultiChoiceState => Double,
      defFn: MultiChoiceState => MultiChoiceState,
      valFnConstructor: () => ValueFunction[MultiChoiceState],
      expandFn: (Int, Set[Int]) => Int,
      computeBudgetFunctionConfig: SelectionComputeBudgetFunctionConfig,
      computeBudgetTestRate: Int
    ): IO[(ValueFunction[MultiChoiceState], NewTree)] = {

      val searchSpaceSize: BigDecimal = choices.map { BigDecimal.apply }.product

      runnerType match {
        case Default =>
          val valFn     = valFnConstructor()
          val computeFn = computeBudgetFunctionConfig.makeComputeBudgetFn(searchSpaceSize)
          val result = IO.fromEither(
            MCTS2.uctSearch(choices, evalFn, defFn, valFn, expandFn, computeFn, computeBudgetTestRate)
          )
          result.map { tree => (valFn, tree) }

        case eb: EnsembleBootstrap =>
          val ensembleComputeFn: (NewTree, Int, Long) => Boolean =
            SelectionComputeBudgetFunctionConfig
              .Samples(eb.samples)
              .makeComputeBudgetFn(searchSpaceSize)

          def bootstrap(
            n: Int = 0,
            bestTreeOption: Option[(ValueFunction[MultiChoiceState], NewTree)] = None
          ): Either[Error, (ValueFunction[MultiChoiceState], NewTree)] = {
            if (n == eb.n) {
              bestTreeOption match {
                case Some(value) => Right(value)
                case None        => Left(new Error("finishing bootstrap with no best tree result"))
              }
            } else {
              val thisValueFn = valFnConstructor()
              val result = for {
                thisTree <- MCTS2.uctSearch(
                  choices,
                  evalFn,
                  defFn,
                  thisValueFn,
                  expandFn,
                  ensembleComputeFn,
                  computeBudgetTestRate
                )
              } yield {
                bestTreeOption match {
                  case None =>
                    bootstrap(n + 1, Some((thisValueFn, thisTree)))
                  case Some((bestVfn, bestTree)) =>
                    val bestPair: (ValueFunction[MultiChoiceState], NewTree) =
                      pickBest(thisValueFn, thisTree, bestVfn, bestTree)

                    bootstrap(n + 1, Some(bestPair))
                }
              }
              result.flatten
            }
          }

          val result = for {
            bestPair <- bootstrap()
          } yield {
            val (vfn, tree) = bestPair
            val computeFn   = computeBudgetFunctionConfig.makeComputeBudgetFn(searchSpaceSize)

            // todo: need version of MCTS2.uctSearch which can be passed a NewTree (in progress)
            ???

            MCTS2
              .uctSearch(choices, evalFn, defFn, vfn, expandFn, computeFn, computeBudgetTestRate)
              .map { t => (vfn, t) }
          }

          IO.fromEither(result.flatten)
      }

    }
  }

  /**
    * measure the better-performing tree search of two options
    * @param vfa first value function
    * @param ta first tree
    * @param vfb second value function
    * @param tb second tree
    * @return pair that has the best (aka, globally-min) value function observations
    */
  def pickBest(
    vfa: ValueFunction[MultiChoiceState],
    ta: NewTree,
    vfb: ValueFunction[MultiChoiceState],
    tb: NewTree
  ): (ValueFunction[MultiChoiceState], NewTree) = {
    vfa match {
      case pra: ValueFunction.PedrosoRei =>
        vfb match {
          case prb: ValueFunction.PedrosoRei =>
            if (pra.globalMinObservation < prb.globalMinObservation) {
              (vfa, ta)
            } else {
              (vfb, tb)
            }
        }
    }
  }
}
