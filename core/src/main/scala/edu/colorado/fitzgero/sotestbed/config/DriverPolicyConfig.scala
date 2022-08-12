package edu.colorado.fitzgero.sotestbed.config

import java.io.File

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy._

sealed trait DriverPolicyConfig

object DriverPolicyConfig {

  /**
    * always bid this exact value
    * @param bid the value to always bid
    */
  case class Fixed(bid: Karma) extends DriverPolicyConfig

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
  case class DelayWithKarmaMapping(unit: Karma, maxBid: Option[Karma]) extends DriverPolicyConfig

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
    *
    * @param maxBid maximum allowed bid
    */
  case class DelayProportional(maxBid: Option[Karma]) extends DriverPolicyConfig

  /**
    * communicates with an RL server via HTTP which provides on-policy actions
    *
    * @param structure the structure of the problem, be it single or multi-agent
    * @param client HTTP client parameters
    */
  case class ExternalRLServer(structure: RLDriverPolicyStructure, client: RLDriverPolicyClient)
      extends DriverPolicyConfig

  /**
    * reads a lookup table from file
    * @param file the file to read from
    * @param balanceCol the column name for the agent's current balance value (discrete)
    * @param urgencyCol the column name for the agent's current urgency value (discrete)
    * @param bidCol the bid to place based on the balance and urgency
    * @param defaultBid the default bid in the case that there is no entry for the driver's balance and urgency
    */
  case class DiscreteLookupTable(file: File, balanceCol: String, urgencyCol: String, bidCol: String, defaultBid: Karma)
      extends DriverPolicyConfig

  implicit class DriverPolicyConfigExtension(driverPolicyConfig: DriverPolicyConfig) {

    def buildDriverPolicy: Either[Error, DriverPolicy] = {
      driverPolicyConfig match {
        case Fixed(bid)               => Right(DriverPolicy.Fixed(bid))
        case p: DelayWithKarmaMapping => Right(DriverPolicy.DelayWithKarmaMapping(p.unit, p.maxBid))
        case p: DelayProportional     => Right(DriverPolicy.DelayProportional(p.maxBid))
        case p: ExternalRLServer      => Right(DriverPolicy.RLBasedDriverPolicy(p.structure, p.client))
        case _: DiscreteLookupTable   => Left(new NotImplementedError("see DriverPolicy.scala file"))
      }
    }
  }
}
