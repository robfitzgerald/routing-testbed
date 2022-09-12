package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

final case class NetworkZone(zoneId: String, edges: List[EdgeId])
