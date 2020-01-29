package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

import scala.collection.immutable.Queue

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, SimTime}

object EdgeBPRUpdateOps {

  /**
    * update a link by replacing the current flow with this flow value
    *
    * @param e a link
    * @param flow the new flow value
    * @return the updated link
    */
  def edgeUpdateWithFlowCount(e: EdgeBPR, flow: Flow): EdgeBPR = {
    val flowUpdate: Flow = Flow(math.max(0, flow.value)) // protect against negatives
    e.copy(
      flow = flowUpdate,
      flowHistory = Queue(flowUpdate),
      flowHistoryLength = 1,
    )
  }

  /**
    * update a link by incrementing/decrementing the flow with this flow delta
    *
    * @param e a link
    * @param flow the incremental flow delta (pos/neg)
    * @return the updated link
    */
  def edgeUpdateWithFlowCountDelta(e: EdgeBPR, flow: Flow): EdgeBPR = {
    val flowUpdate: Flow = Flow(math.max(0, e.flow.value + flow.value)) // protect against negatives
    e.copy(
      flow = flowUpdate,
      flowHistory = Queue.empty,
      flowHistoryLength = 0
    )
  }

  /**
    * update a link which stores a buffer of recent flow counts and averages them into a flow per time value
    *
    * @param flowRateBufferTime number of successive flow values kept in memory and averaged from to get a flow rate
    * @param e a link
    * @param flow the new flow value to add to the buffer. cost flow value will be sampled from the average of buffer values.
    * @return the updated link
    */
  def edgeUpdateWithFlowRate(flowRateBufferTime: SimTime)(e: EdgeBPR, flow: Flow): EdgeBPR = {

    // todo: does not correct downward when average should create a negative slope

    if (e.flowHistoryLength == flowRateBufferTime.value) {
      // flow history buffer is full, will stay at previous length after operation (drop one off the queue)

      // remove the oldest data point from a running average
      // new_average = ((average * nbValues) - value) / (nbValues - 1)
      val oldestValue: Flow = e.flowHistory.head
      val avgWithoutOldest: Flow = ((e.flow * Flow(e.flowHistoryLength)) - oldestValue) / Flow(e.flowHistoryLength- 1)

      // update the vehicle count, which is what is actually enqueued
      // new_average = average + ((value - average) / nbValues)
      // guard against negative values
      val updatedVehicleCount: Flow = Flow(math.max(e.vehicleCount.value + flow.value, 0))
      val nextFlowHistory: Queue[Flow] = e.flowHistory.tail.enqueue(updatedVehicleCount)
      val avgWithLatest: Flow = avgWithoutOldest + ((updatedVehicleCount - avgWithoutOldest) / Flow(e.flowHistoryLength))

      e.copy(
        flow = avgWithLatest,
        flowHistory = nextFlowHistory,
        vehicleCount = updatedVehicleCount
      )

    } else if (e.flowHistoryLength < flowRateBufferTime.value) {
      // haven't fully filled up the flow history buffer yet
      val nextFlowHistoryLength: Int = e.flowHistoryLength + 1

      // update flow-as-count value
      val updatedVehicleCount: Flow = Flow(math.max(e.vehicleCount.value + flow.value, 0))
      val nextFlowHistory: Queue[Flow] = e.flowHistory.enqueue(updatedVehicleCount)
      val avgWithLatest: Flow = e.flow + ((updatedVehicleCount - e.flow) / Flow(nextFlowHistoryLength))

      e.copy(
        flow = avgWithLatest,
        flowHistory = nextFlowHistory,
        flowHistoryLength = nextFlowHistoryLength,
        vehicleCount = updatedVehicleCount
      )
    } else {
      // flowRateBufferTime must have changed to be smaller since last edge update. should be rare or never happen
      // but! if it does, we need to downsize our history and traverse the history to compute the average
      val updatedVehicleCount: Flow = e.vehicleCount + flow
      val nextFlowHistory: Queue[Flow] = e.flowHistory.drop(flowRateBufferTime.value.toInt - 1).enqueue(updatedVehicleCount)
      val revisedAverage: Flow =
        if (nextFlowHistory.nonEmpty) Flow(nextFlowHistory.reduce(_+_).value / flowRateBufferTime.value)
        else Flow.Zero

      e.copy(
        flow = revisedAverage,
        flowHistory = nextFlowHistory,
        flowHistoryLength = nextFlowHistory.length,
        vehicleCount = updatedVehicleCount
      )
    }
  }

  /**
    * update a link which stores a buffer of recent flow counts and averages them into a flow per time value
    *
    * @param decay rate of decay for a flow rate, per SimTime step
    * @param epsilon error below which the flow value is ignored
    * @param e a link
    * @param flow the new flow value to add to the buffer. cost flow value will be sampled from the average of buffer values.
    * @return the updated link
    */
  def edgeUpdateWithMarginalFlowAndDecay(decay: Flow, epsilon: Flow)(e: EdgeBPR, flow: Flow): EdgeBPR = {
    val nextFlowHistory: Queue[Flow] =
      for {
        flowValue <- e.flowHistory.enqueue(flow)
        updatedFlowValue = flowValue * decay
        if updatedFlowValue > epsilon
      } yield updatedFlowValue
    val nextFlowHistoryLength = nextFlowHistory.length

    e.copy(
      flow = Flow(nextFlowHistory.reduce(_+_).value / nextFlowHistoryLength),
      flowHistory = nextFlowHistory,
      flowHistoryLength = nextFlowHistoryLength
    )
  }
}

