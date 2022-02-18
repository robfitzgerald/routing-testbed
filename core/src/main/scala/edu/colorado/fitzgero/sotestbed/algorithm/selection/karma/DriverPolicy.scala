package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.InputStream

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import kantan.csv._
import kantan.csv.ops._

sealed trait DriverPolicy

object DriverPolicy {

  /**
    * always bid this exact value
    * @param bid the value to always bid
    */
  case class Fixed(bid: Karma) extends DriverPolicy

  /**
    * bids karma proportional to delay using some unit karma value
    *
    * computed as max(0, (t_c - t_o / t_o)) * k
    * where
    *  t_o: original trip time estimate
    *  t_c: current (delayed) trip time estimate
    *  k: the unitBidValue which is the amount of karma bid for every
    *     100% increase to travel time
    * @param unit unit bid value for every 100% increase in travel time
    */
  case class DelayWithKarmaMapping(unit: Karma) extends DriverPolicy

  /**
    * bids karma proportional to delay, for experiments where one Karma
    * is bid for each second of delay (and max karma is in the same
    * magnitude to some reasonable max delay for the scenario).
    *
    * computed as max(0, (t_c - t_o / t_o)) * k_t
    * where
    *  t_o: original trip time estimate
    *  t_c: current (delayed) trip time estimate
    *  k_t: agent's current karma balance (at time t)
    */
  case object DelayProportional extends DriverPolicy

  case class LookupTable(table: (Karma, Urgency) => Karma) extends DriverPolicy

  object LookupTable {
    // data class for a lookup table row with header names as fields
    case class Row(balance: Karma, urgency: Urgency, bid: Karma)
    implicit val hd: HeaderDecoder[Row] = HeaderDecoder.decoder("balance", "urgency", "bid") { Row.apply }

    def apply(input: InputStream, precision: Int = 0, defaultValue: Karma = Karma(0.0)): Either[Error, LookupTable] = {
      val readResult = for {
        rows <- ReadResult.sequence(input.readCsv[List, Row](rfc.withHeader("balance", "urgency", "bid")))
      } yield {
        val lookup = rows.map { row =>
          val karmaLookupInternal: Int   = (row.balance.value * precision).toInt
          val urgencyLookupInternal: Int = (row.urgency.value * precision).toInt
          ((karmaLookupInternal, urgencyLookupInternal), row.bid)
        }.toMap
        val lookupFn: (Karma, Urgency) => Karma =
          (karma: Karma, urgency: Urgency) => {
            val karmaQueryInternal: Int   = (karma.value * precision).toInt
            val urgencyQueryInternal: Int = (urgency.value * precision).toInt
            val query: (Int, Int)         = (karmaQueryInternal, urgencyQueryInternal)
            lookup.getOrElse(query, defaultValue)
          }
        LookupTable(lookupFn)
      }

      readResult.left.map { t => new Error("failed reading input as driver policy table", t) }
    }
  }

  implicit class DriverPolicyExtensionMethods(policy: DriverPolicy) {

    def header: String = policy match {
      case _: Fixed                 => "bid"
      case _: DelayWithKarmaMapping => "bid"
      case DelayProportional        => "bid"
      case _: LookupTable           => "bid"
    }

    def applyDriverPolicy(
      requests: List[Request],
      bank: Map[String, Karma],
      activeAgentHistory: ActiveAgentHistory
    ): IO[List[Bid]] =
      policy match {
        case fixed: Fixed =>
          requests.traverse { req =>
            val inner = bank.get(req.agent) match {
              case None => Left(new Error(s"agent ${req.agent} missing from bank"))
              case Some(karmaBalance) =>
                val bid = if (karmaBalance.value < fixed.bid.value) karmaBalance else fixed.bid
                Right(Bid(req, bid))
            }
            IO.fromEither(inner)
          }
        case delayRelative: DelayWithKarmaMapping =>
          requests.traverse { req =>
            val inner = bank.get(req.agent) match {
              case None => Left(new Error(s"agent ${req.agent} missing from bank"))
              case Some(karmaBalance) =>
                val bidOption = for {
                  oldest <- activeAgentHistory.getOldestDataFor(req.agent)
                  latest <- activeAgentHistory.getMostRecentDataFor(req.agent)
                } yield {
                  // as proportional increase, lower bounded by zero
                  val oldestTime = oldest.overallTravelTime.value.toDouble
                  val latestTime = latest.overallTravelTime.value.toDouble
                  val proportion = math.max(0.0, (latestTime - oldestTime) / oldestTime)
                  val karmaToBid = math.min(proportion * delayRelative.unit.value, karmaBalance.value)
                  Bid(req, Karma(karmaToBid))
                }
                bidOption match {
                  case None        => Left(new Error(s"agent ${req.agent} missing from bank or active agent history"))
                  case Some(karma) => Right(karma)
                }
            }
            IO.fromEither(inner)
          }
        case lookup: LookupTable =>
          requests.traverse { req =>
            val inner = bank.get(req.agent) match {
              case None               => Left(new Error(s"agent ${req.agent} missing from bank"))
              case Some(karmaBalance) =>
                // todo: how to source "urgency"? we need LookupTable to have
                //  an argument for an urgency mapping function, (SimTime, SimTime) => Urgency
                val urgency: Urgency = ???
                val bid              = lookup.table(karmaBalance, urgency)
                Right(Bid(req, bid))
            }
            IO.fromEither(inner)
          }
        case DelayProportional =>
          requests.traverse { req =>
            val inner = bank.get(req.agent) match {
              case None => Left(new Error(s"agent ${req.agent} missing from bank"))
              case Some(karmaBalance) =>
                val bidOption = for {
                  oldest <- activeAgentHistory.getOldestDataFor(req.agent)
                  latest <- activeAgentHistory.getMostRecentDataFor(req.agent)
                } yield {
                  // as proportional increase, lower bounded by zero
                  val oldestTime = oldest.overallTravelTime.value.toDouble
                  val latestTime = latest.overallTravelTime.value.toDouble
                  val proportion = math.max(0.0, (latestTime - oldestTime) / oldestTime)
                  val karmaToBid = proportion * karmaBalance.value
                  Bid(req, Karma(karmaToBid))
                }
                bidOption match {
                  case None        => Left(new Error(s"agent ${req.agent} missing from bank or active agent history"))
                  case Some(karma) => Right(karma)
                }
            }
            IO.fromEither(inner)
          }
      }
  }
}
