package edu.colorado.fitzgero.sotestbed.matsim.analysis

import scala.util.Try

case class PGFPlotsData(
  experimentName: String,
  winPct: Double,
  losePct: Double,
  ttNorm: Double,
  ttStdev: Double,
  distNorm: Double,
  distStdev: Double,
  speedNorm: Double,
  speedStdev: Double
)

object PGFPlotsData {

  def pickOrdering(xs: List[PGFPlotsData]): Ordering[PGFPlotsData] = {
    val numeric = xs.map { _.experimentName }.forall { e =>
      Try { e.toDouble }.isSuccess
    }
    if (numeric) {
      Ordering.by[PGFPlotsData, Double] { _.experimentName.toDouble }
    } else {
      Ordering.by[PGFPlotsData, String] { _.experimentName }
    }
  }
}
