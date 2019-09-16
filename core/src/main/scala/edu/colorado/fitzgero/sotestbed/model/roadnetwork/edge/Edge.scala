package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import edu.colorado.fitzgero.sotestbed.model.numeric.Flow

object Edge {
  type UpdateFunction[E] = (E, Flow) => E
}
