package edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge

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
    e.copy(
      flow = flow,
      flowHistory = List(flow),
      flowHistoryLength = 1
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
    e.copy(
      flow = e.flow + flow,
      flowHistory = List(e.flow + flow),
      flowHistoryLength = 1
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
    if (e.flowHistoryLength == flowRateBufferTime.value) {
      // flow history buffer is full, will stay at previous length after operation (drop one off the queue)
      val nextFlowHistory: List[Flow] = flow +: e.flowHistory.dropRight(1)
      e.copy(
        flow = Flow(nextFlowHistory.reduce(_+_).value / flowRateBufferTime.value),
        flowHistory = nextFlowHistory
      )

    } else if (e.flowHistoryLength < flowRateBufferTime.value) {
      // haven't fully filled up the flow history buffer yet
      val nextFlowHistory: List[Flow] = flow +: e.flowHistory
      e.copy(
        flow = Flow(nextFlowHistory.reduce(_+_).value / nextFlowHistory.length),
        flowHistory = nextFlowHistory,
        flowHistoryLength = e.flowHistoryLength + 1
      )
    } else {
      // flowRateBufferTime must have changed to be smaller since last edge update. should be rare or never happen
      val nextFlowHistory: List[Flow] = flow +: e.flowHistory.take(flowRateBufferTime.value.toInt - 1)
      e.copy(
        flow = Flow(nextFlowHistory.reduce(_+_).value / flowRateBufferTime.value),
        flowHistory = nextFlowHistory,
        flowHistoryLength = nextFlowHistory.length
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
  def edgeUpdateWithMarginalFlowAndDecay(decay: Flow, epsilon: Flow = Flow(0.5))(e: EdgeBPR, flow: Flow): EdgeBPR = {
    val nextFlowHistory: List[Flow] =
      for {
        flowValue <- flow +: e.flowHistory
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

