package edu.colorado.fitzgero.sotestbed.algorithm.batching

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.batching.Batching.{BatchingInstruction, BatchingStrategy}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

case class GreedyBatching(
  batchWindow: SimTime,
  minimumReplanningWaitTime: SimTime,
  maxBatchSize: Int
) extends BatchingFunction {

  /**
    * takes the current batching strategy and any updates about replan-able agents, and spits out an
    * update to that batching strategy
    *
    * @param roadNetwork          the current road network state
    * @param currentBatchStrategy the current strategy
    * @param newBatchData         some new data about agents eligible for replanning from the system
    * @param currentTime          the current sim time
    * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
    */
  def updateBatchingStrategy[F[_]: Monad, V, E](roadNetwork: RoadNetwork[F, V, E],
                                                newBatchData: List[AgentBatchData],
                                                currentBatchStrategy: BatchingStrategy,
                                                agentBatchDataMap: Map[String, AgentBatchData],
                                                currentTime: SimTime): F[Option[List[Batching.BatchingInstruction]]] = {
    if (newBatchData.isEmpty) Monad[F].pure {
      None
    } else
      Monad[F].pure {

        // just insert requests at the soonest possible batch time
        val nextBatchingTime: SimTime = BatchingManager.nextValidBatchingTime(batchWindow, currentTime)

        newBatchData
          .filter {
            // remove agents who have met the minimum replanning wait time since their last replanning
            _.lastReplanningTime match {
              case None => true
              case Some(lastReplanningTime) =>
                lastReplanningTime + minimumReplanningWaitTime > currentTime
            }
          } match {
          case Nil         => None
          case newRequests =>
            // we have agents that we can replan to add to the nearest possible request time
            val agentsToAdd: BatchingStrategy = {
              newRequests.map { data =>
                data.request.agent -> BatchingInstruction(data.request.agent, nextBatchingTime, "placeholder")
              }
            }.toMap

            val added: Map[String, BatchingInstruction] =
              agentsToAdd.foldLeft(currentBatchStrategy) {
                case (strat, (id, instruction)) =>
                  strat.updated(id, instruction)
              }

            val asBatches: List[BatchingInstruction] = {
              for {
                (batch, idx) <- added.values.sliding(maxBatchSize, maxBatchSize).zipWithIndex.toList
                batchId = s"$idx"
                instruction <- batch
              } yield instruction.copy(batchId = batchId)
            }

            asBatches match {
              case Nil => None
              case _   => Some { asBatches }
            }
        }
      }
  }
}
