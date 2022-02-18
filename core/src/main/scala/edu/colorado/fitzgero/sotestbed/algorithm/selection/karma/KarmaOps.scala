package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData.EdgeData
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime

object KarmaOps {

  /**
    * compute the urgency of a trip based on experienced delay
    *
    * @param aprioriUrgency urgency the agent had before they began their trip
    * @param firstRoute first route assigned to the agent
    * @param currentShortestPath route generated for the agent at the current time step
    * @return the urgency
    */
  def computeUrgency(
    aprioriUrgency: Double,
    firstRoute: List[EdgeData],
    currentShortestPath: List[EdgeData]
  ): Double = {
    val oldestTripDuration =
      firstRoute.foldLeft(SimTime.Zero) { (acc, link) => acc + link.estimatedTimeAtEdge.getOrElse(SimTime.Zero) }
    val latestTripDuration =
      currentShortestPath.foldLeft(SimTime.Zero) { (acc, link) =>
        acc + link.estimatedTimeAtEdge.getOrElse(SimTime.Zero)
      }
    val urgency = aprioriUrgency + math.max(0, (latestTripDuration - oldestTripDuration).value)
    math.max(0, urgency)
  }

  /**
    * pays out bids from smallest to largest uniformly to all other agents in the sub-batch.
    *
    * if a bid payout would allow a player to exceed the max karma value, the value is
    * truncated (the remainder is retained by the sender).
    * @param bids the bids to transact
    * @param bank the current karma bank balance
    * @param maxKarma the max value of karma allowed in an account
    * @return the updated bank
    */
  def resolveBidsUniformly(
    bids: List[Bid],
    bank: Map[String, Karma],
    maxKarma: Karma
  ): Map[String, Karma] = {
    // first, withdraw the amount each agent has bid from the bank
    val withdrawals =
      bids.foldLeft(bank) { (acc, bid) =>
        val prev = acc.getOrElse(bid.request.agent, Karma(0.0))
        acc.updated(bid.request.agent, prev - bid.value)
      }

    val n                = bids.length
    val agents           = bids.map { _.request.agent }.toSet
    val bidsSmallToLarge = bids.filter { _.value > Karma(0.0) }.sortBy { _.value }

    // redistribute the funds by stepping through each bid and paying slices of
    // the bid amount uniformly across all other agents in the sub-batch
    val redistributed = bidsSmallToLarge.foldLeft(withdrawals) { (acc, bid) =>
      val sender      = bid.request.agent
      val bidPortion  = Karma(bid.value.toDouble / (n - 1))
      val otherAgents = agents - sender

      val recipientUpdate = otherAgents.foldLeft(acc) { (innerAcc, receiver) =>
        val receiverPrev = acc.getOrElse(receiver, Karma(0.0))
        val receiverNext = receiverPrev + bidPortion
        if (receiverNext <= maxKarma) {
          innerAcc.updated(receiver, receiverNext)
        } else {
          // only pay as much as we can to this agent, retain the rest
          val retained   = receiverNext - maxKarma
          val senderPrev = acc.getOrElse(sender, Karma(0.0))
          acc
            .updated(sender, senderPrev + retained)
            .updated(receiver, maxKarma)
        }
      }

      recipientUpdate
    }

    redistributed
  }
}
