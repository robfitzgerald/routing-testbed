package edu.colorado.fitzgero.sotestbed.matsim.io.network

case class BoundingBox(
  minX: Double = Double.MaxValue,
  minY: Double = Double.MaxValue,
  maxX: Double = Double.MinValue,
  maxY: Double = Double.MinValue
) {

  def add(x: Double, y: Double): BoundingBox = {
    this.copy(
      minX = if (x < minX) x else minX,
      minY = if (y < minY) y else minY,
      maxX = if (x > maxX) x else maxX,
      maxY = if (y > maxY) y else maxY
    )
  }

  override def toString: String =
    f"""
       |batching-function {
       |  min-x = $minX%.2f
       |  max-x = $maxX%.2f
       |  min-y = $minY%.2f
       |  max-y = $maxY%.2f
       |}
       |
       |xdist ydist ${maxX - minX}%.2f ${maxY - minY}%.2f
       |""".stripMargin
}
