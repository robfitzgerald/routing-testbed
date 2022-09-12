package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow
import edu.colorado.fitzgero.sotestbed.model.numeric.MetersPerSecond

object Edge {
  type UpdateFunction[E] = (E, Option[Flow], MetersPerSecond) => E
}
