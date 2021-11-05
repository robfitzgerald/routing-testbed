package edu.colorado.fitzgero.sotestbed.matsim.analysis

case class AgentMetrics(
  trial: String,
  experimentName: String,
  overallMetrics: AgentBaseMetrics,
  allPerformanceMetrics: AgentPerformanceMetrics,
  winnerMetrics: AgentPerformanceMetrics,
  loserMetrics: AgentPerformanceMetrics
) {
  override def toString: String = {
    val oM  = overallMetrics.toString
    val aPM = allPerformanceMetrics.toString
    val pPM = winnerMetrics.toString
    val nPM = loserMetrics.toString
    s"$experimentName,$trial,$oM,$aPM,$pPM,$nPM"
  }
}

object AgentMetrics {

  val Header: String = {
    val aPMHeader = AgentPerformanceMetrics.headerWithPrefix("all")
    val wPMHeader = AgentPerformanceMetrics.headerWithPrefix("pos")
    val lPMHeader = AgentPerformanceMetrics.headerWithPrefix("neg")

    s"experimentName,trial,${AgentBaseMetrics.Header},$aPMHeader,$wPMHeader,$lPMHeader"
  }
}
