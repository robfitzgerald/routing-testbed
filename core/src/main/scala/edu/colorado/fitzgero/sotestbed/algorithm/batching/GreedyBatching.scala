package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

case class GreedyBatching (
  batchWindow: SimTime,
  minimumReplanningWaitTime: SimTime,
  maxBatchSize: Int
) extends BatchingFunction {

  /**
    * simply assigns any agents eligible for planning/replanning to the next possible batch
    *
    * @param currentBatchStrategy the current strategy
    * @param newBatchData some new data about agents eligible for replanning from the system
    * @param currentTime the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_] : Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
    currentBatchStrategy: Map[SimTime, List[List[AgentBatchData]]],
    newBatchData: List[AgentBatchData],
    currentTime: SimTime): F[Option[Map[SimTime, List[List[AgentBatchData]]]]] = {
    if (newBatchData.isEmpty) Monad[F].pure {
      None
    } else Monad[F].pure {

      // just insert requests at the soonest possible batch time
      val nextBatchingTime: SimTime = BatchingManager.nextValidBatchingTime(batchWindow, currentTime)

      newBatchData
        .filter{
          // remove agents who have met the minimum replanning wait time since their last replanning
          _.lastReplanningTime match {
            case None => true
            case Some(lastReplanningTime) =>
              lastReplanningTime + minimumReplanningWaitTime > currentTime
          }
        } match {
        case Nil => None
        case newRequests =>
          // we have agents that we can replan to add to the nearest possible request time
          val agentsToAdd: List[List[AgentBatchData]] = newRequests.sliding(maxBatchSize, maxBatchSize).toList

          val updatedBatchingStrategyForNextBatchingTime: List[List[AgentBatchData]] =
            currentBatchStrategy.getOrElse(nextBatchingTime, List.empty) ++ agentsToAdd

          Some {
            currentBatchStrategy.updated(nextBatchingTime, updatedBatchingStrategyForNextBatchingTime)
          }
      }
    }
  }
}