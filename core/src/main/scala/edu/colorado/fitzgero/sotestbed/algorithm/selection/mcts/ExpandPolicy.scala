package edu.colorado.fitzgero.sotestbed.algorithm.selection.mcts

import scala.util.Random

sealed trait ExpandPolicy {
  def expand(slot: Int, choices: Set[Int]): Int
}

object ExpandPolicy {

  final case class RandomExpansion(seed: Option[Long]) extends ExpandPolicy {

    val random = new Random(seed.getOrElse(System.currentTimeMillis))

    def expand(slot: Int, choices: Set[Int]): Int = {
      val choice = random.shuffle(choices.toList).head
      choice
    }
  }

  final case object ByPathOrdering extends ExpandPolicy {

    def expand(slot: Int, choices: Set[Int]): Int = {
      choices.min
    }
  }
}
