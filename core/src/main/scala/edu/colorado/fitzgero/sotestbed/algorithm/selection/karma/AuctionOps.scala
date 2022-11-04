package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps

import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching._
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path

object AuctionOps extends LazyLogging {

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
    * agents receiving system optimal route plans shouldn't have to pay into the auction
    *
    * to achieve this, any trip with a trip index not equal to zero has it's bid value
    * reset to zero (recall trip index 0 is the true shortest path).
    *
    * 2022-11-04 update: winners don't pay other winners.
    *
    * @param selectedRoutes all bids, the selected path index, and the path. a path
    *                       index of 0 is a "user-optimal" path.
    * @param bank the current bank ledger
    * @param maxKarma maximum amount of karma an agent can possess at any time
    * @return the updated bank, or, an error
    */
  def resolveBidsWinnersPayAll(
    selectedRoutes: List[(Bid, Int, Path)],
    bank: Map[String, Karma],
    maxKarma: Karma
  ): Either[Error, Map[String, Karma]] = {
    def bothWinners(b1: Bid, b2: Bid): Boolean = b1.value != Karma.Zero && b2.value != Karma.Zero
    val agents                                 = selectedRoutes.map { case (b, _, _) => b.request.agent }
    val onlyWinnersPaying = selectedRoutes
      .map {
        case (bid, idx, _) if idx != 0 => bid.copy(value = Karma.Zero)
        case (bid, _, _)               => bid
      }
      .combinations(2)
      .filter {
        case b1 :: b2 :: Nil if bothWinners(b1, b2) => false
        case _                                      => true
      }
      .map {
        case b1 :: b2 :: Nil => (b1, b2)
      }
    resolveBidsUniformly(onlyWinnersPaying, agents, bank, maxKarma)
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
    bidPairs: Iterator[(Bid, Bid)],
    agents: List[String],
    bank: Map[String, Karma],
    maxKarma: Karma
  ): Either[Error, Map[String, Karma]] = {
    if (agents.lengthCompare(1) <= 0) Right(bank)
    else {
      // first, withdraw the amount each agent has bid from the bank
      val numAgents                                           = agents.length
      val initial: Either[Error, (Map[String, Karma], Karma)] = Right((bank, Karma.Zero))
      val transactionResults = bidPairs.foldLeft(initial) {
        case (acc, (bid1, bid2)) =>
          acc.flatMap {
            case (innerBank, unallocatedKarma) =>
              logger.debug(f"bidA: $bid1 | bidB: $bid2 | bank size ${innerBank.size}")
              val transactionResult = resolveTransactionsInBankWithRemainingUnallocatedFunds(
                bank = innerBank,
                agentA = bid1.request.agent,
                bidA = bid1.value,
                agentB = bid2.request.agent,
                bidB = bid2.value,
                numAgents = numAgents,
                max = maxKarma
              )
              transactionResult.map {
                case (b, r) =>
                  logger.debug(f"after ${bid1.request.agent}, ${bid2.request.agent} transaction, $r unallocated")
                  (b, r + unallocatedKarma)
              }
          }
      }

      transactionResults.flatMap {
        case (innerBank, unallocated) =>
          if (unallocated == Karma.Zero) Right(innerBank)
          else {
            // somehow re-distribute these unallocated funds. working up
            // from the agents with the least karma, we can give them each a
            // portion until we are clear (sounds like a recursive fn)
            redistributeUnallocatedKarma(innerBank, agents, unallocated, maxKarma)
          }
      }
    }
  }

  /**
    * resolves the bids these two agents made, updating the bank. if
    * either transaction leads to an agent's balance exceeding "max",
    * we pass karma back to the original agent. however, if that agent
    * cannot receive this remainder, it is held aside as remaining
    * unallocated funds to be redistributed to the broader group at a
    * later point.
    *
    * @param bank the bank to update
    * @param agentA an agent in a transaction
    * @param bidA the bid associated with that agent
    * @param agentB another agent in a transaction
    * @param bidB the bid associated with the other agent
    * @param numAgents total number of agents in auction
    * @param max maximum karma balance allowed for any agent
    * @return the updated bank and any unallocated karma, or, an error
    */
  def resolveTransactionsInBankWithRemainingUnallocatedFunds(
    bank: Map[String, Karma],
    agentA: String,
    bidA: Karma,
    agentB: String,
    bidB: Karma,
    numAgents: Int,
    max: Karma
  ): Either[Error, (Map[String, Karma], Karma)] = {
    val emptyBids = bidA == Karma.Zero && bidB == Karma.Zero
    val sameBid   = bidA == bidB
    if (emptyBids || sameBid) {
      Right((bank, Karma.Zero))
    } else if (numAgents == 1) {
      val str = s"A: $agentA $bidA | B: $agentB $bidB"
      Left(new Error(s"resolve transactions between 2 agents but numAgents == 1; state: ($str)"))
    } else {
      val result = for {
        balanceA <- BankOps.getBalance(bank, agentA)
        balanceB <- BankOps.getBalance(bank, agentB)
      } yield {
        // compute the portion of the bid each agent should transact here
        // todo: maybe just compute the transaction and remainders and then
        //  call BankOps.transact here instead of reproducing that logic

        logger.debug(f"agent $agentA initial balance $balanceA bid $bidA")
        logger.debug(f"agent $agentB initial balance $balanceB bid $bidB")
        val portionA     = Karma(math.floor(bidA.value / (numAgents - 1)).toLong)
        val portionB     = Karma(math.floor(bidB.value / (numAgents - 1)).toLong)
        val transactionA = -portionA + portionB
        val transactionB = -portionB + portionA
        logger.debug(f"agent $agentA transacting $transactionA")
        logger.debug(f"agent $agentB transacting $transactionB")
        val (updatedBalanceA, remainderA) = balanceAndRemainder(balanceA, transactionA, max)
        val (updatedBalanceB, remainderB) = balanceAndRemainder(balanceB, transactionB, max)
        // it is possible that the bid transaction exceeded the allowed maximum,
        // in which case we need to attempt to give back the remainder to the sender.
        val updatedBalanceAWithRemainder = updatedBalanceA + remainderB
        val updatedBalanceBWithRemainder = updatedBalanceB + remainderA
        val (finalBalanceA, finalRemainderA) =
          if (updatedBalanceAWithRemainder <= max) (updatedBalanceAWithRemainder, Karma.Zero)
          else (max, updatedBalanceAWithRemainder - max)
        val (finalBalanceB, finalRemainderB) = {
          if (updatedBalanceBWithRemainder <= max) (updatedBalanceBWithRemainder, Karma.Zero)
          else (max, updatedBalanceBWithRemainder - max)
        }
        logger.debug(f"final balance limited to $max")
        logger.debug(
          f"agent $agentA updated balance $updatedBalanceA + remainder $remainderB => final balance $finalBalanceA"
        )
        logger.debug(
          f"agent $agentB updated balance $updatedBalanceB + remainder $remainderA => final balance $finalBalanceB"
        )

        if (finalBalanceA < Karma.Zero) {
          Left(new Error(s"agent $agentA final balance after transaction is negative"))
        } else if (finalBalanceB < Karma.Zero) {
          Left(new Error(s"agent $agentB final balance after transaction is negative"))
        } else {
          val updatedBank = bank
            .updated(agentA, finalBalanceA)
            .updated(agentB, finalBalanceB)
          val thisRemainder = finalRemainderA + finalRemainderB
          Right((updatedBank, thisRemainder))
        }
      }
      result.flatten
    }
  }

  /**
    * helper function for
    * @param startBalance
    * @param transaction
    * @param max
    * @return
    */
  def balanceAndRemainder(
    startBalance: Karma,
    transaction: Karma,
    max: Karma
  ): (Karma, Karma) = {
    val updatedBalance = startBalance + transaction
    if (updatedBalance <= max) (updatedBalance, Karma.Zero)
    else {
      val remainder = updatedBalance - max
      (max, remainder)
    }
  }

  /**
    * redistributes unallocated funds. required for maintaining a fixed
    * zero-sum auction, that we don't leak funds during the auction when
    * agents are due payouts from bids but are maxxed out. those agents
    * return their extra funds to this pool which is distributed back to the group.
    *
    * redistribution sorts the agents by their remaining bank headroom (the max amount
    * less their balance), ascending, and then attempts to uniformly re-distribute
    * the unallocated funds in a greedy traversal of the sorted agents.
    *
    * @param bank
    * @param agents
    * @param unallocated
    * @param max
    * @return
    */
  def redistributeUnallocatedKarma(
    bank: Map[String, Karma],
    agents: List[String],
    unallocated: Karma,
    max: Karma
  ): Either[Error, Map[String, Karma]] = {
    if (unallocated == Karma.Zero) {
      logger.debug("no unallocated funds")
      Right(bank)
    } else {

      // track each agent's balance and headroom
      case class Row(agent: String, balance: Karma, headroom: Karma)
      val headroomPerAgentAscendingOrError = agents
        .traverse { agent =>
          BankOps
            .getBalance(bank, agent)
            .map { bal => Row(agent, bal, max - bal) }
        }
        .map { _.sortBy { _.headroom } }

      @tailrec
      def _redistribute(
        headroomPerAgentAscending: List[Row],
        remaining: Karma = unallocated,
        bankUpdates: Map[String, Karma] = bank
      ): Map[String, Karma] = {
        if (remaining == Karma.Zero) bankUpdates
        else
          headroomPerAgentAscending match {
            case Nil                                   => bankUpdates
            case Row(agent, balance, headroom) :: tail =>
              // find the uniform payout split at this point in the traversal, and bound
              // that split by the agent's remaining headroom
              val payoutTargetAmount: Long = remaining.value / headroomPerAgentAscending.length
              val payoutAmount             = Karma(math.min(payoutTargetAmount, headroom.value))

              val updatedRemaining = remaining - payoutAmount
              val updatedBalance   = balance + payoutAmount

              logger.debug(f"agent $agent receiving $payoutAmount as redistribution funds")
              logger.debug(
                f"agent $agent prev balance $balance headroom $headroom (max $max) next balance $updatedBalance"
              )
              val updatedBank = bank.updated(agent, updatedBalance)
              _redistribute(tail, updatedRemaining, updatedBank)
          }
      }

      val resultOrError = headroomPerAgentAscendingOrError.map { _redistribute(_) }

      resultOrError
    }

  }
}
