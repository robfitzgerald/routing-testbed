package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

object RewardOps {
  val AlmostZero                  = 0.000000001
  val InvalidTravelTimeAllocation = 0.0

  def freeFlowSpeedDiffAllocation(travelTime: Double, freeFlowTravelTime: Double, distance: Double): Double = {
    if (travelTime == 0.0 || freeFlowTravelTime == 0.0) InvalidTravelTimeAllocation
    else {
      val speed   = distance / travelTime
      val ffSpeed = distance / freeFlowTravelTime
      // low values are bad, when speed is much less than free flow speed
      // high values are good, when speed is equal to or better than free flow speed
      val alloc = if (speed == 0.0) InvalidTravelTimeAllocation else speed / ffSpeed
      alloc
    }
  }
}
