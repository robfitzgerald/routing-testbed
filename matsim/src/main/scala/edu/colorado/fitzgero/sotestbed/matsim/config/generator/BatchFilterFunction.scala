package edu.colorado.fitzgero.sotestbed.matsim.config.generator

sealed trait BatchFilterFunction

object BatchFilterFunction {
  case class TopK(k: Int) extends BatchFilterFunction
  case object NoFilter    extends BatchFilterFunction

  implicit class BatchFilterFunctionOps(bff: BatchFilterFunction) {

    def toHocon: String = bff match {
      case TopK(k) =>
        s"""algorithm.batch-filter-function = {
           |  type = top-k-ranking-batch-filter
           |  k = $k
           |  batch-overlap-function = {
           |    type = agent-proportional 
           |    path-overlap-lookup-type.type = tsp 
           |    overlap-cost-type.type = non-normalized
           |    use-link-weights = true
           |  }
           |}""".stripMargin
      case NoFilter => "algorithm.batch-filter-function.type = no-filter"
    }
  }
}
