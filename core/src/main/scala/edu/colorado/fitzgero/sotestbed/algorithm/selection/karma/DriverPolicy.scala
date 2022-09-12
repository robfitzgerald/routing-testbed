package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.InputStream

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.RayRLlibClient
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest._
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse._
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.KarmaSelectionRlOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.PathSegment
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicyStructure._

sealed trait DriverPolicy

object DriverPolicy {

  /**
    * always bid this exact value
    * @param bid the value to always bid
    */
  case class Fixed(bid: Karma) extends DriverPolicy

  /**
    * bids karma proportional to delay, for experiments where one Karma
    * is bid for each second of delay (and max karma is in the same
    * magnitude to some reasonable max delay for the scenario).
    *
    * computed as min(1, max(0, (t_c - t_o / t_o))) * k_t
    * where
    *  t_o: original trip time estimate
    *  t_c: current (delayed) trip time estimate
    *  k_t: agent's current karma balance (at time t)
    *
    * @param maxBid maximum allowed bid
    */
  case class DelayProportional(maxBid: Option[Karma]) extends DriverPolicy

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
    * @param maxBid maximum allowed bid
    */
  case class DelayWithKarmaMapping(unit: Karma, maxBid: Option[Karma]) extends DriverPolicy

  case class RLBasedDriverPolicy(structure: DriverPolicyStructure, client: RayRLlibClient) extends DriverPolicy

  case class InterpLookupTable(table: (Karma, Urgency) => Karma) extends DriverPolicy

  object InterpLookupTable {
    // data class for a lookup table row with header names as fields
    case class Row(balance: Karma, urgency: Urgency, bid: Karma)
    implicit val hd: HeaderDecoder[Row] = HeaderDecoder.decoder("balance", "urgency", "bid") { Row.apply }

    def apply(
      input: InputStream,
      precision: Int = 0,
      defaultValue: Karma = Karma.Zero
    ): Either[Error, InterpLookupTable] = {
      val readResult = for {
        rows <- ReadResult.sequence(input.readCsv[List, Row](rfc.withHeader("balance", "urgency", "bid")))
      } yield {
        val lookup = rows.map { row =>
          val karmaLookupInternal: Int   = (row.balance.value * precision).toInt
          val urgencyLookupInternal: Int = (row.urgency.value * precision).toInt
          ((karmaLookupInternal, urgencyLookupInternal), row.bid)
        }.toMap

        // todo:
        //  - finish implementing sotestbed.util.RangeMap
        //  - build two RangeMaps here
        //    - one from Karma (balance) => Urgency
        //    - one from Urgency => Karma (bid)
        //    - lookupFn becomes a nested query to both RangeMaps with a default value of ...? Zero probs.
        val lookupFn: (Karma, Urgency) => Karma = ???

//        val lookupFn: (Karma, Urgency) => Karma =
//          (karma: Karma, urgency: Urgency) => {
//            val karmaQueryInternal: Int   = (karma.value * precision).toInt
//            val urgencyQueryInternal: Int = (urgency.value * precision).toInt
//            val query: (Int, Int)         = (karmaQueryInternal, urgencyQueryInternal)
//            lookup.getOrElse(query, defaultValue)
//          }
        InterpLookupTable(lookupFn)
      }

      readResult.left.map { t => new Error("failed reading input as driver policy table", t) }
    }
  }

  implicit class DriverPolicyExtensionMethods(policy: DriverPolicy) {

    def header: String = policy match {
      case _: Fixed                 => "bid"
      case _: DelayWithKarmaMapping => "bid"
      case _: DelayProportional     => "bid"
      case _: InterpLookupTable     => "bid"
      case _: RLBasedDriverPolicy   => "bid"
    }

    def applyDriverPolicy(
      signal: NetworkPolicySignal,
      alts: Map[Request, List[Path]],
      bank: Map[String, Karma],
      activeAgentHistory: ActiveAgentHistory,
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      costFunction: EdgeBPR => Cost,
      episodePrefix: String,
      episodeId: Option[EpisodeId],
      logFn: Option[(PolicyClientRequest, PolicyClientResponse) => IO[Unit]] = None
    ): IO[List[Bid]] =
      policy match {

        case fixed: Fixed =>
          val result: IO[List[Bid]] = alts.keys.toList.traverse { req =>
            val inner: Either[Error, Bid] = bank
              .get(req.agent)
              .toRight(new Error(s"agent ${req.agent} not found in bank"))
              .map { karmaBalance =>
                val bid = Karma(math.min(karmaBalance.value, fixed.bid.value))
                Bid(req, bid)
              }
            IO.fromEither(inner)
          }
          result

        case DelayProportional(maxBid) =>
          alts.keys.toList.traverse { req =>
            val bidIO: IO[Bid] = for {
              karmaBalance <- IO.fromEither(
                bank.get(req.agent).toRight(new Error(s"agent ${req.agent} not found in bank"))
              )
              oldest     <- IO.fromEither(activeAgentHistory.getOldestRequestOrError(req.agent))
              latest     <- IO.fromEither(activeAgentHistory.getNewestRequestOrError(req.agent))
              oldestTime <- IO.fromEither(oldest.overallTravelTimeEstimate)
              latestTime <- IO.fromEither(latest.overallTravelTimeEstimate)
            } yield {
              // as proportional increase, lower bounded by zero
              // calculated in real numbers, then discretized back to Karma at the end
              val oldestTimeD = oldestTime.value.toDouble
              val latestTimeD = latestTime.value.toDouble
              val delayProportion =
                if (oldestTimeD == 0.0) 0.0
                else math.min(1.0, math.max(0.0, (latestTimeD - oldestTimeD) / oldestTimeD))

              val upperBidValue = maxBid match {
                case Some(mb) => math.min(mb.value, karmaBalance.value)
                case None     => karmaBalance.value
              }
              val karmaBidTarget = delayProportion * upperBidValue

              Bid(req, Karma(karmaBidTarget.toLong))
            }

            bidIO
          }

        case DelayWithKarmaMapping(unit, maxBid) =>
          alts.keys.toList.traverse { req =>
            val bidIO: IO[Bid] = for {
              karmaBalance <- IO.fromEither(
                bank.get(req.agent).toRight(new Error(s"agent ${req.agent} not found in bank"))
              )
              oldest     <- IO.fromEither(activeAgentHistory.getOldestRequestOrError(req.agent))
              latest     <- IO.fromEither(activeAgentHistory.getNewestRequestOrError(req.agent))
              oldestTime <- IO.fromEither(oldest.overallTravelTimeEstimate)
              latestTime <- IO.fromEither(latest.overallTravelTimeEstimate)
            } yield {
              // as proportional increase, lower bounded by zero
              // calculated in real numbers, then discretized back to Karma at the end
              val oldestTimeD = oldestTime.value.toDouble
              val latestTimeD = latestTime.value.toDouble
              val delayProportion =
                if (oldestTimeD == 0.0) 0.0
                else math.min(1.0, math.max(0.0, (latestTimeD - oldestTimeD) / oldestTimeD))

              val upperBidValue = maxBid match {
                case Some(mb) => math.min(mb.value, karmaBalance.value)
                case None     => karmaBalance.value
              }
              val karmaBid = math.min(delayProportion * unit.value, upperBidValue)

              Bid(req, Karma(karmaBid.toLong))
            }

            bidIO
          }

        case rl: RLBasedDriverPolicy =>
          rl.structure match {
            case map: MultiAgentPolicy =>
              IO.fromOption(episodeId)(new Error(s"multiagent scenario missing EpisodeId"))
                .flatMap { epId =>
                  KarmaSelectionRlOps.getMultiAgentBids(
                    client = rl.client,
                    structure = map,
                    episodeId = epId,
                    alts = alts,
                    signal = signal,
                    bank = bank,
                    activeAgentHistory = activeAgentHistory,
                    logFn = logFn
                  )
                }

            case sap: SingleAgentPolicy =>
              KarmaSelectionRlOps.getSingleAgentBids(
                client = rl.client,
                structure = sap,
                episodePrefix = episodePrefix,
                alts = alts,
                signal = signal,
                bank = bank,
                activeAgentHistory = activeAgentHistory
              )
          }

        case lookup: InterpLookupTable =>
          alts.keys.toList.traverse { req =>
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
      }
  }
}
