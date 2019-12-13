package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData

final case class RouteRequests(timeOfDay: Double, requests: List[AgentBatchData])
