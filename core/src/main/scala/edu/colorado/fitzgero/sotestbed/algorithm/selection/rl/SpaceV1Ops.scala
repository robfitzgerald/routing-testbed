package edu.colorado.fitzgero.sotestbed.algorithm.selection.rl

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm.SelectionCost
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, MetersPerSecond}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.rllib.Observation.MultiAgentObservation
import edu.colorado.fitzgero.sotestbed.rllib.Reward.MultiAgentReward
import edu.colorado.fitzgero.sotestbed.rllib.{Action, AgentId, Grouping, Observation, Reward}

/**
  * Observation and Action space v1
  *
  * OBSERVATIONS
  * representation: {{{ {"agent_1": [<srcX>, <srcY>, <dstX>, <dstY>, <congestion>], ... } }}}
  *
  * uses Euclidean space to represent the agent's current location and final destination
  * as coordinates.
  * "congestion" is the % over free speed at the agent's link (or, # cars / link capacity),
  * which helps us observe the network state using local information.
  *
  * ACTIONS
  * representation: {{{ {"agent_1": 0.3, ...} }}}
  * since we don't know the number of alt paths for each agent, we may not want to use a discrete
  * representation for action. here, we try using a percentage [0, 1] which is then used to sample
  * from the true number of available alternative paths.
  *
  * REWARDS
  * from our experiments using heuristics, we know that our cost function has been useful in
  * predicting system-optimal route assignments, even if the cost function is only itself an
  * approximation of the queueing effects in MATSim. it is not the ground truth though, we
  * would need to wait until the end of an episode and compare SO routes with selfish routes
  * in order to know the true travel time improvement.
  *
  * V1 here trains based on the estimated reward represented by the inverse of the agent
  * estimated travel time reduction (since we are maximizing reward in rllib).
  */
object SpaceV1Ops {

  def encodeObservation(costFunction: EdgeBPR => Cost)(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    agents: Map[Request, List[Path]]
  ): Map[AgentId, List[Double]] = {
    val result = for {
      (request, _) <- agents
      agentId = AgentId(request.agent)
      currentEdge          <- roadNetwork.edge(request.location).unsafeRunSync
      currentDstNodeId     <- roadNetwork.destination(currentEdge.edgeId).unsafeRunSync
      currentDstNode       <- roadNetwork.vertex(currentDstNodeId).unsafeRunSync
      destinationEdge      <- roadNetwork.edge(request.destination).unsafeRunSync
      destinationSrcNodeId <- roadNetwork.source(destinationEdge.edgeId).unsafeRunSync
      destinationSrcNode   <- roadNetwork.vertex(destinationSrcNodeId).unsafeRunSync
    } yield {
      // holy crap, that's a lot of work, who made that graph data structure? :-P

      val ffCost = currentEdge.attribute.freeFlowCost
      val congestion = if (ffCost == Cost.Zero) {
        0.0
      } else {
        val curCost = costFunction(currentEdge.attribute)
        ((curCost - ffCost) / ffCost).value
      }

      val obs = List(
        currentDstNode.attribute.x,
        currentDstNode.attribute.y,
        destinationSrcNode.attribute.x,
        destinationSrcNode.attribute.y,
        congestion
      )

      agentId -> obs
    }

    result
  }

  /**
    * decodes the selected actions. if the action selects an out of bounds path id,
    * return the last path id (index of path.last)
    * @param actions
    * @param agents
    * @return
    */
  def decodeAction(actions: Map[AgentId, Int], agents: Map[Request, List[Path]]): Map[AgentId, Int] = {
    val result = for {
      (req, paths) <- agents
      agentId = AgentId(req.agent)
      selectedRouteIndex <- actions.get(agentId)
      truncatedRouteIndex = if (selectedRouteIndex >= paths.length) paths.length - 1 else selectedRouteIndex
    } yield agentId -> truncatedRouteIndex
    result
  }

  def computeReward(
    selfish: SelectionCost,
    optimal: SelectionCost,
    agents: Map[Request, List[Path]]
  ): Map[AgentId, Double] = {
    val result = for {
      ((req, _), (selfish, optimal)) <- agents.zip(selfish.agentPathCosts.zip(optimal.agentPathCosts))
    } yield AgentId(req.agent) -> (selfish.value - optimal.value)
    result.toMap
  }
}
