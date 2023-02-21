package edu.colorado.fitzgero.sotestbed.algorithm.batching

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.{RouteRequestData, SOAgentArrivalData}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection._
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
import scala.annotation.tailrec

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
    * reads through the latest set of messages, and updates the ActiveAgentHistory
    * so that it is current.
    *
    * @param updates the updated AgentBatchData
    * @return the manager with all AgentBatchData updated
    */
  def updateAgentBatchData(
    updates: List[AgentBatchData],
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR]
  ): IO[BatchingManager] = {
    IO.fromEither(BatchingManager.updateBatchingManager(this, updates, minRequestUpdateThreshold))
  }

  /**
    * update the history to show that these requests were assigned replanning routes
    * and tag which ones were UO assignments
    *
    * @param responses responses sent to simulation
    * @return either updated BM or error
    */
  def incrementReplannings(responses: List[Response]): Either[Error, BatchingManager] = {
    val initial: Either[Error, ActiveAgentHistory] = Right(this.storedHistory)
    val updated = responses.foldLeft(initial) {
      case (acc, res) =>
        val uoPathAssigned = res.pathIndex == 0
        acc.flatMap { _.incrementReplannings(res.request.agent, uoPathAssigned) }
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

object BatchingManager extends LazyLogging {

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

  /**
    * updates the state of the batching manager based on the incoming messages from
    * the simulation.
    *
    * @param batchingManager current batching manager state to update
    * @param updates the updates to apply, a series of messages for possibly many agents.
    *                these should have already been filtered to remove invalid state, such
    *                as submitting route requests after the agent has reached their destination.
    * @param minRequestUpdateThreshold a threshold to prevent too-frequent updates
    * @return the effect of updating the state of the BatchingManager
    */
  def updateBatchingManager(
    batchingManager: BatchingManager,
    updates: List[AgentBatchData],
    minRequestUpdateThreshold: SimTime
  ): Either[Error, BatchingManager] = {

    @tailrec
    def _update(remaining: List[AgentBatchData], b: BatchingManager): Either[Error, BatchingManager] = {
      remaining match {
        case Nil => Right(b)
        case head :: tail =>
          head match {

            case data: EnterSimulation =>
              // agent has departed, start storing their history
              b.storedHistory
                .processDepartureFor(data) match {
                case Left(err) => Left(err)
                case Right(updatedHist) =>
                  val updated = b.copy(storedHistory = updatedHist)
                  _update(tail, updated)
              }

            case data: SOAgentArrivalData =>
              // agent has arrived, log this data and stop tracking this trip

              val result = for {
                agentHistory <- b.storedHistory.getAgentHistoryOrError(data.agentId)
                originalTT   <- agentHistory.first.tripTravelTimeEstimate
                _ <- Try {
                  val replannings      = agentHistory.replanningEvents
                  val uoRoutesAssigned = agentHistory.uoPathsAssigned
                  val freeFlowTT       = data.finalFreeFlowTravelTime
                  val row              = TripLogRow(data, originalTT, freeFlowTT, replannings, uoRoutesAssigned)
                  b.tripLog.write(row.toString + "\n")
                  b.tripLog.flush()
                }.toEither.left.map { t => new Error(t) }
              } yield b.copy(
                batchData = b.batchData - data.agentId,
                storedHistory = b.storedHistory.processArrivalFor(
                  data.agentId,
                  data.arrivalTime
                )
              )

              result match {
                case Left(error)    => Left(error)
                case Right(updated) => _update(tail, updated)
              }

            case data: RouteRequestData =>
              // make sure we aren't updating their history "too frequently" (inclusive inequality)
              // and also make sure we aren't beginning to store RouteRequestData before observing
              // an EnterSimulation message for this agent
              val enterSimulationMessageNotObserved = !b.storedHistory.hasHistoryForAgent(data.agent)
              val canUpdate =
                if (enterSimulationMessageNotObserved) false
                else {
                  val optionalNewestRequest = b.storedHistory.getNewestRequest(data.request.agent)
                  optionalNewestRequest match {
                    case None => true
                    case Some(lastData) =>
                      val withinUpdateThreshold =
                        data.timeOfRequest - lastData.timeOfRequest >= minRequestUpdateThreshold
                      withinUpdateThreshold
                  }
                }
              val updateResult =
                if (!canUpdate) Right(b)
                else
                  b.storedHistory.processRouteRequestData(data).map { processed =>
                    b.copy(
                      batchData = b.batchData.updated(data.request.agent, data),
                      storedHistory = processed
                    )
                  }

              // val result = for {
              //   mostRecent <- b.storedHistory.getNewestDataOrError(data.request.agent)
              //   canUpdate = data.timeOfRequest - mostRecent.timeOfRequest >= minRequestUpdateThreshold
              // } yield {
              //   if (!canUpdate) Right(b)
              //   else
              //     b.storedHistory.processRouteRequestData(data).map { processed =>
              //       b.copy(
              //         batchData = b.batchData.updated(data.request.agent, data),
              //         storedHistory = processed
              //       )
              //     }
              // }
              updateResult match {
                case Left(error) =>
                  logger.error("updates:")
                  updates.foreach { u => logger.error(s"${u.getClass.getSimpleName} - ${u.agent}") }
                  logger.error("updates sorted:")
                  updates.sorted.foreach { u => logger.error(s"${u.getClass.getSimpleName} - ${u.agent}") }
                  val agentStateOrEmpty =
                    b.storedHistory.observedRouteRequestData.get(data.agent).map(_.toString).getOrElse("<empty>")
                  logger.error(s"batching manager state for agent ${data.agent}: $agentStateOrEmpty")
                  Left(error)
                case Right(updated) => _update(tail, updated)
              }

          }

      }
    }

    _update(updates.sorted, batchingManager)
  }
}
