package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

trait BatchingFunction {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork the current road network state
    * @param currentBatchStrategy the current strategy
    * @param newBatchData some new data about agents eligible for replanning from the system
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
                                         currentBatchStrategy: Map[SimTime, Map[String, AgentBatchData]],
                                         newBatchData: List[AgentBatchData],
                                         currentTime: SimTime): F[Option[Map[SimTime, Map[String, AgentBatchData]]]]

}
