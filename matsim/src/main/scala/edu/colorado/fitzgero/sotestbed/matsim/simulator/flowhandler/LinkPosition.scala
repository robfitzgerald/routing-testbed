package edu.colorado.fitzgero.sotestbed.matsim.simulator.flowhandler

import org.hisrc.w3c.atom.v_1_0.Link

final case class LinkPosition(value: Double) extends AnyVal

object LinkPosition {

  val Start = LinkPosition(0.0)
  val End   = LinkPosition(1.0)

}
