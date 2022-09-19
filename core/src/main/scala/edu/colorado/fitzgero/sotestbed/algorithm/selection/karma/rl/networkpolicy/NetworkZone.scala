package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId

/**
  * associates a Batch with a group of edges in the road network.
  * these edges
  *   1. are observed for this Batch's congestion observation
  *   2. are used to group Agents
  *
  * @param batchId the unique identifier for this batch from the batching manager
  *                and batching function
  * @param edges
  */
final case class NetworkZone(batchId: String, edges: List[EdgeId])
