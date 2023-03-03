package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import edu.colorado.fitzgero.sotestbed.model.agent.Request

case class Bid(request: Request, value: Karma) {
  override def toString: String = f"${request.agent} bid $value"
}
