package edu.colorado.fitzgero.sotestbed.matsim.config.generator

object Default {

  def apply(bprAlpha: Double = 0.15, bprBeta: Double = 4.0): String =
    s"""use-free-flow-network-costs-in-path-search = true
       |io.routing-report-config.type = all-aggregate
       |io.matsim-log-level = "WARN"
       |
       |run {
       |  start-of-sim-time = 0
       |  end-of-sim-time = 86400
       |  end-of-routing-time = 86399
       |  matsim-step-size = 1.0
       |  matsim-semaphore-timeout-ms = 60000
       |  simulation-tail-timeout = "2 minutes"
       |}
       |
       |algorithm {
       |  edge-update-function.type = flow-count
       |  path-to-marginal-flows-function.type = default
       |  combine-flows-function.type = sum
       |  marginal-cost-function = {
       |    type = edge-bpr-function
       |    alpha = $bprAlpha
       |    beta = $bprBeta
       |  }
       |}
       |""".stripMargin
}
