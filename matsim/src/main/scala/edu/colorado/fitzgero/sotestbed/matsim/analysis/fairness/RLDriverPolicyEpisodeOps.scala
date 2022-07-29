package edu.colorado.fitzgero.sotestbed.matsim.analysis.fairness

import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import java.nio.file.Path
import java.time.LocalTime
import edu.colorado.fitzgero.sotestbed.matsim.analysis.AgentExperienceRow
import kantan.csv._
import kantan.csv.ops._
import scala.util.Try

object RLDriverPolicyEpisodeOps {

  /**
    * compares the travel time of each trip from a reference run. computes
    * the fairness of the travel time diff using the user-perceived variant of
    * Jain's Fairness Index.
    *
    * for RL models which are not multi-agent. this assumes that, for each
    * (AgentId, Reward) pair, we are logging separate returns to some known
    * and shared policy.
    *
    * here, we only concern ourselves with minimizing unfairness. if a result is
    * better than fair (i.e., improves travel time), we report it as a reward of
    * zero. otherwise, we report a negative reward in [-1, 0) which captures the
    * unfair allocation with respect to the group.
    *
    * @param config
    * @param referenceExperiment
    * @return
    */
  def generateEpisodeRewardFromSelfishComparison(
    config: MATSimRunConfig,
    referenceExperiment: String = "selfish",
    agentExperienceFile: String = "agentExperience.csv",
    fairnessFunction: Seq[Double] => Option[Seq[Double]] = JainFairnessMath.userFairness
  ): IO[List[(String, Reward)]] = {
    val selfishDir  = config.experimentDirectory.getParent.resolve(referenceExperiment)
    val selfishFile = selfishDir.resolve(agentExperienceFile).toFile
    val optimalFile = config.experimentDirectory.resolve(agentExperienceFile).toFile

    val result = for {
      s     <- ReadResult.sequence(selfishFile.asCsvReader[AgentExperienceRow](rfc.withHeader).toList)
      o     <- ReadResult.sequence(optimalFile.asCsvReader[AgentExperienceRow](rfc.withHeader).toList)
      diffs <- computeDiffs(s, o)
    } yield {
      val (agentIds, agentDiffs) = diffs.unzip

      // compute the range of unfairness results
      val unfairDiffs = agentDiffs.map {
        case diff if diff > 0.0 => 0.0
        case diff               => diff
      }

      JainFairnessMath.userFairness(agentDiffs) match {
        case None =>
          // the mean fairness was zero which is not
          List.empty[(String, Reward)]
        case Some(userFairness) =>
          agentIds.zip(userFairness.map { Reward.SingleAgentReward.apply })
      }
    }
    IO.fromEither(result)
  }

  /**
    * generates a reward for each agent which is computed via the user-perceived fairness
    * equation.
    *
    * @param diffs travel time diff from reference to result, computed as (ref - res). if
      the result is faster than the reference, the value is positive.
    * @return
    */
  def generateSingleAgentRewards(diffs: Seq[(String, Double)]): Either[Error, List[(String, Reward)]] = {

    val (agentIds, agentDiffs) = diffs.unzip

    // compute the range of unfairness results - ignore any magnitude of travel time improvement here.
    // we flip (negate) the diff value, as Jain's index is defined only for distributions with a
    // positively-valued mean. we will flip the result afterward.
    val unfairDiffs = agentDiffs.map {
      case diff if diff > 0.0 => 0.0
      case diff               => -diff
    }

    JainFairnessMath.userFairness(unfairDiffs) match {
      case None =>
        // the mean fairness was zero; all rewards are zero
        val zeroRewards = agentIds.toList.map { case a => (a, Reward.SingleAgentReward(0.0)) }
        Right(zeroRewards)
      case Some(userFairness) =>
        // shift the result by 1 to the left on the number line, so that rewards are in
        // the range [-1, 0]
        val rewards = agentIds.toList.zip(userFairness.map { uf => Reward.SingleAgentReward(uf - 1) })
        Right(rewards)
    }
  }

  def generateMultiAgentRewards(): IO[Reward] = ???

  /**
    * match all agent id / trip combinations between the two files
    * and compute the difference in travel time for each match.
    *
    * these appear in the order sorted by the index of (AgentId, TripNumber), so that
    * it is lexicographical by agent id and increasing by trip number, such as:
    * > List(("a", 1), ("a", 0), ("b", 2)).sorted
    * produces
    * > List[(String, Int)] = List(("a", 0), ("a", 1), ("b", 2))
    *
    * @param selfish the selfish rows
    * @param optimal the optimal rows
    * @return the diffs (optimal - selfish) or an error
    */
  def computeDiffs(
    selfish: List[AgentExperienceRow],
    optimal: List[AgentExperienceRow]
  ): Either[Error, List[(String, Double)]] = {
    val result = for {
      sIndexed <- createAgentAndTripIndex(selfish)
      oIndexed <- createAgentAndTripIndex(optimal)
    } yield {
      val sLookup = sIndexed.sortBy { case (index, _) => index }.toMap
      oIndexed.traverse {
        case (oIdx, oRow) =>
          sLookup.get(oIdx) match {
            case None =>
              val msg = s"agent/trip idx from optimal run not found in selfish run: $oIdx"
              Left(new Error(msg))
            case Some(sRow) =>
              val (agentId, _) = oIdx
              val diff         = oRow.travelTime - sRow.travelTime
              Right((agentId, diff))
          }
      }
    }

    result.flatten
  }

  /**
    * agent experience rows are listed by agentId and departure time,
    * but the departure times may vary between files when comparing.
    * in order to properly compare rows, we need to generate the trip
    * index for each row (the sequential ordering value for each trip
    * for each agent).
    *
    * @param rows the rows to annotate
    * @return rows indexed by agent id and trip index, starting from 1.
    */
  def createAgentAndTripIndex(
    rows: List[AgentExperienceRow]
  ): Either[Error, List[((String, Int), AgentExperienceRow)]] = {
    val withTimes = rows.traverse { row =>
      Try {
        val t = LocalTime.parse(row.departureTime)
        ((row.agentId, t), row)
      }
    }
    val withIdx = withTimes.map {
      _.groupBy { case ((agentId, _), _) => agentId }.flatMap {
        case (agentId, timeRows) =>
          timeRows
            .sortBy {
              case ((_, t), _) =>
                t
            }
            .zipWithIndex
            .map {
              case (((_, _), row), idx) =>
                // start from 1 instead of 0
                ((agentId, idx + 1), row)
            }

      }.toList
    }

    withIdx.toEither.left.map { t => new Error("error parsing local time from agent row") }
  }

}
