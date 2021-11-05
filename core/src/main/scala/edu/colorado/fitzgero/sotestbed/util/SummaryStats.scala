package edu.colorado.fitzgero.sotestbed.util

case class SummaryStats(
  m1: Double = 0.0,
  m2: Double = 0.0,
  m3: Double = 0.0,
  m4: Double = 0.0,
  n: Int = 0
) {

  def mean: Double = m1

  def variance: Option[Double] = if (n < 2) None else Some { m2 / (n - 1.0) }

  def standardDeviation: Option[Double] = this.variance.map { math.sqrt }

  def skewness: Option[Double] = if (m2 == 0.0) None else Some { math.sqrt(n.toDouble) * m3 / math.pow(m2, 1.5) }

  def kurtosis: Option[Double] = {
    val denom: Double = (m2 * m2) - 3.0
    if (denom <= 0.0) None else Some { n.toDouble * m4 / denom }
  }

  def add(observation: Double): SummaryStats = {

    val nextN: Int       = n + 1
    val delta: Double    = observation - m1
    val delta_n: Double  = delta / nextN
    val delta_n2: Double = delta_n * delta_n
    val term1: Double    = delta * delta_n * n

    SummaryStats(
      m1 = m1 + delta_n,
      m2 = m2 + term1,
      m3 = m3 + term1 * delta_n * (n - 2) - 3 * delta_n * m2,
      m4 = m4 + term1 * delta_n2 * (n * n - 3 * n + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3,
      n = n + 1
    )
  }

  def +(that: SummaryStats): SummaryStats = {
    val combinedN = this.n + that.n

    val delta: Double  = that.m1 - this.m1
    val delta2: Double = delta * delta
    val delta3: Double = delta * delta2
    val delta4: Double = delta2 * delta2

    val combinedM1: Double = (this.n * this.m1 + that.n * that.m1) / combinedN
    val combinedM2: Double = this.m2 + that.m2 + delta2 * this.n * that.n / combinedN
    val deltaM3: Double    = 3.0 * delta * (this.n * that.m2 - that.n * this.m2) / combinedN
    val combinedM3: Double = (this.m3 + that.m3 + delta3 * this.n * that.n * (this.n - that.n) / (combinedN * combinedN)) + deltaM3
    val deltaM4
      : Double = 6.0 * delta2 * (this.n * this.n * that.m2 + that.n * that.n * this.m2) / (combinedN * combinedN) + 4.0 * delta * (this.n * that.m3 - that.n * this.m3) / combinedN
    val combinedM4
      : Double = (this.m4 + that.m4 + delta4 * this.n * that.n * (this.n * this.n - this.n * that.n + that.n * that.n) / (combinedN * combinedN * combinedN)) + deltaM4

    SummaryStats(
      m1 = combinedM1,
      m2 = combinedM2,
      m3 = combinedM3,
      m4 = combinedM4,
      n = combinedN
    )
  }

  override def toString: String = {
    val meanValue = f"$mean%.2f"
    val standardDeviationValue = standardDeviation
      .map { s =>
        f"$s%.2f"
      }
      .getOrElse("")
    val skewnessValue = skewness
      .map { s =>
        f"$s%.2f"
      }
      .getOrElse("")
    val kurtosisValue = kurtosis
      .map { s =>
        f"$s%.2f"
      }
      .getOrElse("")
    s"$meanValue,$standardDeviationValue,$skewnessValue,$kurtosisValue"
  }

}
