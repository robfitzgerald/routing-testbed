package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import scala.util.Random

import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithmHelper

trait DefaultPolicy {

  def policy(multiChoice: MultiChoice, rnal: SelectionAlgorithmHelper)(
    multiChoiceState: MultiChoiceState
  ): MultiChoiceState
}

object DefaultPolicy {

  case class UniformRandomPolicy(seed: Option[Long]) extends DefaultPolicy {

    val random = new Random(seed.getOrElse(System.currentTimeMillis))

    def policy(multiChoice: MultiChoice, rnal: SelectionAlgorithmHelper)(
      multiChoiceState: MultiChoiceState
    ): MultiChoiceState = {
      val partialIdx = multiChoiceState.size
      (partialIdx until rnal.choices.length).foldLeft(multiChoiceState) { (acc, idx) =>
        val nChoicesForIdx = rnal.choices(idx)
        val choice         = random.nextInt(nChoicesForIdx)
        acc.addChoiceToState(multiChoice.statePartitionOffsets, idx, choice)
      }
    }
  }
}
