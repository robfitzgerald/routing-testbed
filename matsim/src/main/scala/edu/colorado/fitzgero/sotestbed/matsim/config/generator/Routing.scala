package edu.colorado.fitzgero.sotestbed.matsim.config.generator

object Routing {
  val SecondsInDay: Int = 86400

  def apply(
    adoptionRate: Double,
    batchWindow: Int,
    maxPathAssignments: Int = SecondsInDay,
    replanningRequestUpdateIterationCycle: Int = 1,
    networkUpdateIterationCycle: Int = 1,
    bprAlpha: Double = 0.15,
    bprBeta: Double = 4.0
  ): String = {
    val minReqUpdateThreshold = batchWindow * replanningRequestUpdateIterationCycle
    val minNetUpdateThreshold = batchWindow * networkUpdateIterationCycle
    s"""routing {
       |  adoption-rate = $adoptionRate
       |  batch-window = $batchWindow
       |  max-path-assignments = $maxPathAssignments                             
       |  minimum-replanning-lead-time = $batchWindow                            
       |  min-batch-size = 2                                                     
       |  min-batch-search-space = 0                                             
       |  minimum-replanning-wait-time = $batchWindow                            
       |  minimum-average-improvement = 0                                        
       |  min-request-update-threshold = $minReqUpdateThreshold                  
       |  min-network-update-threshold = $minNetUpdateThreshold                  
       |  selfish {
       |    type = dijkstra
       |    path-to-marginal-flows-function.type = default
       |    combine-flows-function.type = sum
       |    marginal-cost-function = {
       |      type = edge-bpr-function
       |      alpha = $bprAlpha
       |      beta = $bprBeta
       |    }
       |  }
       |}""".stripMargin
  }
}
