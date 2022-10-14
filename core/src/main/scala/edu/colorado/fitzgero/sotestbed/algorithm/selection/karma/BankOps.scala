package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Ordering.Implicits.infixOrderingOps
import cats.implicits._

trait BankOps {

  // deserves a different home
  implicit class MapOps[V](map: Map[String, V]) {

    def getOrError(key: String): Either[Error, V] =
      map
        .get(key)
        .toRight {
          val top5 = map.take(5).mkString("\n")
          new Error(s"key $key missing from lookup with entries (top 5):\n$top5")
        }
  }

  implicit class AgentMapOps(bank: Map[String, Karma]) {

    def getOrError(agent: String): Either[Error, Karma] =
      bank
        .get(agent)
        .toRight {
          new Error(s"agent $agent missing from lookup")
        }

    def limitBidByBalance(bid: Karma, agent: String): Either[Error, Karma] =
      bank
        .getOrError(agent)
        .map { balance =>
          val bidTrunc = math.min(bid.value, balance.value)
          Karma(bidTrunc)
        }
  }

}

object BankOps {

  def getBalance(bank: Map[String, Karma], agent: String): Either[Error, Karma] =
    bank
      .get(agent)
      .toRight(new Error(s"bank missing agent $agent"))

  /**
    * perform a transaction against an agent's balance
    *
    * @param bank the bank state
    * @param agent the agent to transact
    * @param transaction a withdrawal (negative) or deposit (positive)
    * @param max max karma value
    * @return updated bank or error
    */
  def transact(
    bank: Map[String, Karma],
    agent: String,
    transaction: Karma,
    max: Karma
  ): Either[Error, Map[String, Karma]] = {
    bank.get(agent) match {
      case None =>
        Left(new Error(s"agent $agent attempting to transact $transaction but is not in bank"))
      case Some(balance) =>
        val updatedBalance = balance + transaction
        if (updatedBalance < Karma.Zero) {
          Left(new Error(s"agent $agent attempting to transact $transaction but went negative"))
        } else if (updatedBalance > max) {
          Left(new Error(s"agent $agent attempting to transact $transaction but exceeded max $max"))
        } else {
          Right(bank.updated(agent, balance + transaction))
        }
    }
  }

  def getBalance(bank: Map[String, Karma])(bid: Bid): Either[Error, (Bid, Karma)] =
    bank
      .get(bid.request.agent)
      .toRight(new Error(s"bank missing agent ${bid.request.agent}"))
      .map { k => (bid, k) }

  /**
    * withdraw the bid amount from the bank for each agent bid
    *
    * @param bank the bank to withdraw from
    * @param bids the bids to withdraw
    * @param max karma max value
    * @return
    */
  def processWithdrawals(bank: Map[String, Karma], bids: List[Bid], max: Karma): Either[Error, Map[String, Karma]] = {
    val initial: Either[Error, Map[String, Karma]] = Right(bank)
    bids.foldLeft(initial) { (acc, bid) =>
      val agent       = bid.request.agent
      val transaction = -bid.value
      acc.flatMap { innerBank => transact(innerBank, agent, transaction, max) }
    }
  }
}
