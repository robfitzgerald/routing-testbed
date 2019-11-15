package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

// todo: should a RoutingExperiment have a "optimizeBatches/assignBatches" step between "advance" and "getActiveRequests"?

class BatchManager (
  batchingFunction: (BatchManager, List[Request], SimTime) => BatchManager,
  batches: Map[SimTime, Map[String, BatchManager.AgentBatchData]] = Map.empty,
  batchWindow: SimTime
) {

}

object BatchManager {
  final case class AgentBatchData(
    request: Request,
    currentRoute: List[EdgeId] // todo: should this also hold any other data, like travel time estimates?
  )
}