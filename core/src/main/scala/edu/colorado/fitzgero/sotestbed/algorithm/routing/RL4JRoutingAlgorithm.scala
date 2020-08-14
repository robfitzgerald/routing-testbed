package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.SyncIO

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR

class RL4JRoutingAlgorithm[V](model: RoutingAlgorithm[SyncIO, V, EdgeBPR], agent: Nothing) extends RoutingAlgorithm[SyncIO, V, EdgeBPR] {

  // todo:
  //  1. try building rl4j and including in the lib directory
  //  2. let's roll our own Q-Learning, and maybe MCTS too

  def route(requests: List[Request],
            activeAgentHistory: ActiveAgentHistory,
            roadNetwork: RoadNetwork[SyncIO, V, EdgeBPR]): SyncIO[RoutingAlgorithm.Result] = ???

}
