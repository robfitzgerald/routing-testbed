package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.SelectionRunnerResult
import edu.colorado.fitzgero.sotestbed.model.agent.{RequestClass, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import java.io.PrintWriter
import scala.util.Try
import java.nio.file.Path
import scala.util.Failure
import scala.util.Success
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.EnterSimulation

final case class BatchingManager(
  batchWindow: SimTime,
  minRequestUpdateThreshold: SimTime,
  costFunction: EdgeBPR => Cost,
  batchData: Map[String, RouteRequestData],
  storedHistory: ActiveAgentHistory,
  mostRecentBatchRequested: SimTime,
  tripLog: PrintWriter
) extends LazyLogging {

  /**
    * reads through the latest set of requests, and updates the collection of AgentBatchData
    * so that it is current. if any agent instructions stored are stale, we remove it.
    *
    * invariant: if an agent's data is stored in the manager but does not come via the updates,
    * then it is stale and removed.
    *
    * @param updates the updated AgentBatchData
    * @return the manager with all AgentBatchData updated, and stale instructions removed
    */
  def updateAgentBatchData(
    updates: List[AgentBatchData],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[BatchingManager] = {

    updates.foldLeft(IO.pure(this)) { (ioB, data) =>
      ioB.flatMap { b =>
        data match {

          case EnterSimulation(agent, departureTime) =>
            IO {
              logger.info(s"recognizing agent $agent entered simulation at $departureTime")
              b
            }
          case data: SOAgentArrivalData =>
            // log this data and stop tracking this trip
            storedHistory.getOldestRequest(data.agentId) match {
              case None =>
                // agent hasn't been in the system long enough to have been
                // replanned at all
                IO {
                  val tt = data.finalTravelTime
                  val msg = f"agent ${data.agentId} arriving before receiving any plans with " +
                    f"overall travel time of $tt"
                  logger.info(msg)
                  b
                }
              case Some(oldestData) =>
                for {
                  originalTT <- IO.fromEither(oldestData.overallTravelTimeEstimate)
                  _ = IO.pure {
                    if (originalTT > SimTime(3600)) {
                      storedHistory.observedRouteRequestData.get(data.agentId).foreach { stored =>
                        val replCnt = stored.replanningEvents
                        val rows = stored.orderedEntryHistory.map {
                          case AgentHistory.Entry(h, replan) =>
                            val auditExp =
                              if (h.experiencedRoute.isEmpty) SimTime.Zero
                              else SimTime(h.experiencedRoute.flatMap { _.estimatedTimeAtEdge.map { _.value } }.sum)
                            val auditRem =
                              if (h.remainingRoute.isEmpty) SimTime.Zero
                              else SimTime(h.remainingRoute.flatMap { _.estimatedTimeAtEdge.map { _.value } }.sum)
                            val time        = h.timeOfRequest
                            val experienced = h.experiencedTravelTime
                            val remaining = h.remainingTravelTimeEstimate match {
                              case Left(value) =>
                                "<NA>"
                              case Right(rem) =>
                                rem.toString
                            }
                            f"$time: $experienced $remaining $replan $replCnt $auditExp $auditRem"
                        }
                        val detail = rows.mkString("\n")
                        logger.info {
                          f"""
                             |reporting outlier trip log output for agent ${data.agentId} who finished their
                             |trip at time ${data.arrivalTime}. original travel time estimate was quoted at
                             |$originalTT which is greater than 1 hour. 
                             |$$timeOfRequest: $$experiencedTravelTime $$remainingTravelTime $$replanned $$replanningCount $$auditExperienced $$auditRemaining
                             |$detail""".stripMargin
                        }
                      }

                    }
                  }
                  _ <- IO.fromTry(Try {
                    val row = TripLogRow(data, originalTT)
                    this.tripLog.write(row.toString + "\n")
                    this.tripLog.flush()
                  })
                } yield {
                  b.copy(
                    batchData = b.batchData - data.agentId,
                    storedHistory = b.storedHistory.processArrivalFor(data.agentId)
                  )
                }
            }

          case data: RouteRequestData =>
            val canUpdateRequest =
              b.storedHistory.getNewestRequest(data.request.agent) match {
                case None =>
                  true
                case Some(mostRecent) =>
                  // guard against the requestUpdateCycle
                  data.timeOfRequest - mostRecent.timeOfRequest >= minRequestUpdateThreshold
              }

            if (canUpdateRequest) {

              for {
                processed <- b.storedHistory.processRouteRequestData(data, roadNetwork, costFunction)
              } yield this.copy(
                batchData = b.batchData.updated(data.request.agent, data),
                storedHistory = processed
              )
            } else IO.pure(b)

        }
      }
    }
  }

  def incrementReplannings(responses: List[Response]): Either[Error, BatchingManager] = {
    val initial: Either[Error, ActiveAgentHistory] = Right(this.storedHistory)
    val updated = responses.foldLeft(initial) {
      case (acc, res) =>
        acc.flatMap { _.incrementReplannings(res.request.agent) }
    }
    updated.map { updatedHist => this.copy(storedHistory = updatedHist) }
  }

  /**
    * if it is the right time to request an SO batch of requests, then we do that
    *
    * detail: maybe we want to prevent instantaneous routing solutions in the future,
    * so we can demonstrate the online and realtime accuracy of the solution,
    * but for now, we can just let this happen, since the time a replanning
    * event is computed is less of a constraint under this formulation.
    *
    * @param currentTime the current sim time
    * @return either an SO route request (and batch manager update) or no change
    */
  def submitActiveRouteRequestsForReplanning(currentTime: SimTime): (BatchingManager, List[RouteRequestData]) = {
    BatchingManager.mostRecentBatchTime(this.batchWindow, currentTime) match {
      case None => (this, List.empty)
      case Some(aBatchTimePossiblySkipped) =>
        if (mostRecentBatchRequested < aBatchTimePossiblySkipped) {

          // we just passed a batch time or landed on one. export the batch. clear the batch of requests.
          val batchingManagerAfterExport: BatchingManager = this.copy(
            mostRecentBatchRequested = aBatchTimePossiblySkipped,
            batchData = Map.empty
          )
          (batchingManagerAfterExport, batchData.values.toList)
        } else {

          // haven't hit batch request time yet, so, hold on to the batch for now.
          (this, List.empty)
        }
    }
  }

  def close(): IO[Unit] = {
    IO.fromTry(Try {
      logger.info(s"closing ${BatchingManager.TripLogFilename}")
      this.tripLog.close()
    })
  }
}

object BatchingManager {

  val TripLogFilename = "tripLog.csv"

  def apply(
    batchWindow: SimTime,
    minRequestUpdateThreshold: SimTime,
    costFunction: EdgeBPR => Cost,
    outputDirectory: Path
  ): IO[BatchingManager] = {
    for {
      uri <- IO.fromTry(Try { outputDirectory.resolve(TripLogFilename) })
      pw <- IO.fromTry(Try {
        val pw = new PrintWriter(uri.toFile)
        pw.write(TripLogRow.Header + "\n")
        pw
      })
    } yield {
      val bm = BatchingManager(
        batchWindow = batchWindow,
        minRequestUpdateThreshold = minRequestUpdateThreshold,
        costFunction = costFunction,
        batchData = Map.empty,
        storedHistory = ActiveAgentHistory(),
        mostRecentBatchRequested = SimTime.Zero,
        tripLog = pw
      )
      bm
    }
  }

  def splitUEFromSO(data: AgentBatchData): Boolean = {
    data match {
      case data: AgentBatchData.RouteRequestData => data.request.requestClass == RequestClass.UE
      case _                                     => false
    }
  }

  /**
    * inspects the output of routing algorithms and makes sure there exists one response per agent
    * by only taking the best-cost solution for each agent
    * @param routingResult the result of independent routing algorithm calls for this [[SimTime]]
    * @return the route responses sent to the simulator
    */
  def resolveRoutingResultBatches(routingResult: List[RoutingAlgorithm.Result]): List[Response] =
    routingResult
      .flatMap { _.responses }
      .groupBy { _.request.agent }
      .map { case (_, solutionsForAgent: List[Response]) => solutionsForAgent.minBy { _.costEstimate } }
      .toList

  /**
    * gives us the most recently-completed batch window time
    * @param batchWindow duration of batches
    * @param currentTime the current sim time
    * @return the most recently-completed batch time, or None if currentTime is negative
    */
  def mostRecentBatchTime(batchWindow: SimTime, currentTime: SimTime): Option[SimTime] = {
    if (currentTime < SimTime.Zero) None
    else {
      val batchOfDay: Int = math.floor(currentTime.value.toDouble / batchWindow.value.toDouble).toInt
      Some { SimTime(batchOfDay) * batchWindow }
    }
  }

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
  def nextValidBatchingTime(batchWindow: SimTime, currentTime: SimTime): SimTime = {
    if (currentTime < SimTime.Zero) {
      // end of the first batch window will suffice for negative time values
      // (of which -1 should be the only one)
      batchWindow
    } else {
      // find the SimTime at the end of the next batch
      ((currentTime / batchWindow) * batchWindow) + (SimTime(2) * batchWindow)
    }
  }

  /**
    * if the updated strategy has invalid batch entries, list them
    * @param newStrategy the new strategy
    * @param batchWindow batch window for
    * @param currentTime the current sim time
    * @return a list of any entries found to be invalid (hopefully empty!)
    */
  def listInvalidStrategies[A](
    newStrategy: Map[SimTime, List[List[A]]],
    batchWindow: SimTime,
    currentTime: SimTime
  ): Seq[SimTime] = {
    for {
      t <- currentTime until nextValidBatchingTime(batchWindow, currentTime)
      time = SimTime(t)
      if newStrategy.isDefinedAt(time)
    } yield {
      time
    }
  }

  /**
    * keeps only the soonest batch, who should not be tampered with
    * @param oldStrat the old strategy
    * @param currentTime the current SimTime
    * @param batchWindow the batch window
    * @return the strategy with only the scheduled Requests for the nearest batch time
    */
  def retainStrategyForNearestBatchTime(
    oldStrat: Map[SimTime, List[List[AgentBatchData]]],
    currentTime: SimTime,
    batchWindow: SimTime
  ): Map[SimTime, List[List[AgentBatchData]]] = {
    val nearestBatchTime: SimTime = nextValidBatchingTime(batchWindow, currentTime)
    oldStrat.filter { case (simTime, _) => simTime == nearestBatchTime }
  }
}
