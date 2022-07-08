package edu.colorado.fitzgero.sotestbed.matsim.config.generator

sealed trait AltPathsFunction

object AltPathsFunction {
  case class KSPWithLimitedOverlap(k: Int, theta: Double, searchIterations: Int) extends AltPathsFunction

  implicit class AltPathsFunctionOps(altPathsFunction: AltPathsFunction) {

    def toHocon: String = altPathsFunction match {
      case KSPWithLimitedOverlap(k, theta, searchIterations) =>
        s"""algorithm.ksp-algorithm = {
           |  type = svp-lo-sync
           |  k = $k
           |  theta = $theta
           |  min-batch-size = 2
           |  marginal-cost-function = {
           |    type = edge-bpr-function
           |    alpha = 0.15
           |    beta = 4
           |  }
           |  ksp-termination-function = {
           |    type = paths-seen
           |    seen = $searchIterations
           |  }
           |}""".stripMargin
    }
  }
}
