package edu.colorado.fitzgero.sotestbed.matsim.reporting

case class PerformanceMetrics(
  ttDiffMin: Double = 0.0,
  ttDiffNorm: Double = 0.0,
  distDiffMiles: Double = 0.0,
  distDiffNorm: Double = 0.0,
  speedDiffMph: Double = 0.0,
  speedDiffNorm: Double = 0.0,
  count: Int = 0,
  metricsSampledRatio: Double = 0.0
) {

  def avg: PerformanceMetrics = PerformanceMetrics(
    ttDiffMin = this.ttDiffMin / count,
    ttDiffNorm = this.ttDiffNorm / count,
    distDiffMiles = this.distDiffMiles / count,
    distDiffNorm = this.distDiffNorm / count,
    speedDiffMph = this.speedDiffMph / count,
    speedDiffNorm = this.speedDiffNorm / count,
    count
  )

  def addMetricsSampledPct(metricsSampledRatio: Double): PerformanceMetrics =
    this.copy(metricsSampledRatio = metricsSampledRatio)

  override def toString: String = {
    val ttDiffNormString: String     = PerformanceMetrics.ratioToPercent(ttDiffNorm)
    val distDiffNormString: String   = PerformanceMetrics.ratioToPercent(distDiffNorm)
    val speedDiffNormString: String  = PerformanceMetrics.ratioToPercent(speedDiffNorm)
    val metricsSampledString: String = PerformanceMetrics.ratioToPercent(metricsSampledRatio)
    f"$ttDiffMin%.2f,$ttDiffNormString,$distDiffMiles%.2f,$distDiffNormString,$speedDiffMph%.2f,$speedDiffNormString,$metricsSampledString"
  }

}

object PerformanceMetrics {
  val Header: String = "ttDiffMin,ttDiffNorm,distDiffMiles,distDiffNorm,speedDiffMph,speedDiffNorm,metricsSamplePct"

  def ratioToPercent(n: Double): String = f"${n * 100.0}%.2f%%"
}
