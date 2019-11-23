package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Meters, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId


case class BatchingManager (
  batchWindow     : SimTime,
  batchingStrategy: Map[SimTime, Map[String, BatchingManager.AgentBatchData]] = Map.empty,
) extends LazyLogging {

  /**
    * looks up any batch for this sim time and return it
    * @param currentSimTime the current sim time
    * @return the updated BatchingManager along with any requests for this sim time
    */
  def getBatchForTime(currentSimTime: SimTime): (BatchingManager, List[Request]) = {
    val nextBatch = batchingStrategy
      .get(currentSimTime)
      .map{_.values.map{_.request}.toList}
      .getOrElse(List.empty)
    val updatedBatchingManager =
      this.copy(
        batchingStrategy = batchingStrategy - currentSimTime
      )
    (updatedBatchingManager, nextBatch)
  }

  /**
    * takes a new batching strategy, and if it is valid, updates the batching manager;
    * if invalid (a batch assignment occurs for a time sooner than could properly be handled
    * in simulation time), then no update occurs and the error is logged (fails silently).
    * @param newBatchStrategy a new batching strategy from a BatchingFunction
    * @param currentTime the current sim time
    * @return the updated batching manager (or the same one)
    */
  def updateBatchData(newBatchStrategy: Option[Map[SimTime, Map[String, BatchingManager.AgentBatchData]]], currentTime: SimTime): BatchingManager =
    newBatchStrategy match {
      case None => this
      case Some(newStrat) =>
        val nextValidBatchingTime: SimTime = BatchingManager.nextValidBatchingTime(batchWindow, currentTime)
        val validStrategy: Boolean = {
          val found: Seq[Boolean] = for {
            t   <- currentTime.value to nextValidBatchingTime.value
            time = SimTime(t)
            if newStrat.isDefinedAt(time)
          } yield {
            logger.error(s"batching strategy update at time $currentTime has invalid time $time; ignoring update at time")
            false
          }
          found.forall(identity)
        }
        if (validStrategy) {
          this.copy(batchingStrategy=newStrat)
        } else {
          this
        }
    }
}

object BatchingManager {

  final case class AgentBatchData(
    request: Request,
    currentRoute: List[EdgeId],
    estimatedTravelTime: TravelTimeSeconds,
    estimatedDistance: Meters,
    lastReplanningTime: SimTime,
  )

  /**
    * it is invalid to set a batch time for agents unless a complete
    * batch window exists between now and that time. this function
    * gives the next valid batch time based on the current time and
    * the batch window
    *
    * @param batchWindow configuration parameter
    * @param currentTime the current sim time
    * @return the next valid replanning batch time that can be set
    */
  def nextValidBatchingTime(batchWindow: SimTime, currentTime: SimTime): SimTime =  currentTime + (currentTime % batchWindow) + batchWindow
}