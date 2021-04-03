package edu.colorado.fitzgero.sotestbed.matsim.config.generator

sealed trait AssignmentAlgorithm

object AssignmentAlgorithm {

  case object Base                                                                     extends AssignmentAlgorithm
  case class Rand(computeBudgetMs: Int, subBatchK: Int, exploredBudget: Double = 0.01) extends AssignmentAlgorithm
  case class Mcts(computeBudgetMs: Int, subBatchK: Int, exploredBudget: Double = 0.01) extends AssignmentAlgorithm

//  def randomPick(random: Random, computeBudgetMs: Int, subBatchK: Int, exploredBudget: Double): AssignmentAlgorithm = {
//    random.nextInt(4) match {
//      case 0 => Selfish
//      case 1 => Base
//      case 2 => Rand(computeBudgetMs, subBatchK, exploredBudget)
//      case 3 => Mcts(computeBudgetMs, subBatchK, exploredBudget)
//      case n => throw new IllegalStateException(s"random next int $n should not be possible")
//    }
//  }

  implicit class AlgorithmOps(algorithm: AssignmentAlgorithm) {

    def algorithmName: String = algorithm match {
      case Base    => "base"
      case _: Rand => "rand"
      case _: Mcts => "mcts"
    }

    def toHocon: String = algorithm match {
      case Base =>
        s"""algorithm {
           |  type = system-optimal
           |  name = "base"
           |  selection-algorithm.type = tsp-selection
           |}""".stripMargin
      case Rand(computeBudgetMs, subBatchK, exploredBudget) =>
        val millis = computeBudgetMs / subBatchK
        s"""algorithm = {
           |  type = system-optimal
           |  name = "rand2"
           |  selection-algorithm = {
           |    type = "random-selection-2"
           |    seed = 0
           |    exhaustive-search-sample-limit = 1
           |    compute-budget-function-config = {
           |      type = "duration-ms-and-percent-explored"
           |      milliseconds = $millis
           |      explored = $exploredBudget
           |    }
           |    compute-budget-test-rate = 100
           |  }
           |}""".stripMargin
      case Mcts(computeBudgetMs, subBatchK, exploredBudget) =>
        val millis = computeBudgetMs / subBatchK
        s"""algorithm = {
           |  type = system-optimal
           |  name = "mcts2"
           |  selection-algorithm = {
           |    type =local-mcts-2
           |    seed = 0
           |    minimum-average-batch-travel-improvement = 0.0
           |    exhaustive-search-sample-limit = 1
           |    agent-ordering.type = "batch-proportional"
           |    expand-policy.type = "random-expansion"
           |    compute-budget-function-config = {
           |      type = "duration-ms-and-percent-explored"
           |      milliseconds = $millis
           |      explored = $exploredBudget
           |    }
           |    compute-budget-test-rate = 100
           |  }
           |}""".stripMargin
    }
  }
}
