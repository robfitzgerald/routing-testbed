package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.rllib._
import java.nio.file.Path
import java.time.LocalTime
// import edu.colorado.fitzgero.sotestbed.matsim.analysis.AgentExperienceRow
import kantan.csv._
import kantan.csv.ops._
import scala.util.Try
import scala.annotation.tailrec
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.SOAgentArrivalData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.BatchingManager
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.algorithm.batching.TripLogRow
import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpace
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma

object RLDriverPolicyEpisodeOps extends LazyLogging {

  /**
    * grab the [[TripLogRow]]s from the experiment logging directory
    *
    * @param experimentDirectory the directory where the final
    * trip log should be found
    * @return the effect of reading the tripLog.csv file
    */
  def getTripLog(experimentDirectory: Path): IO[List[TripLogRow]] = {
    val tripLogFile = BatchingManager.TripLogFilename
    val uri         = experimentDirectory.resolve(tripLogFile).toFile
    val rowsResult  = ReadResult.sequence(uri.asCsvReader[TripLogRow](rfc.withHeader).toList)
    IO.fromEither(rowsResult)
  }

  /**
    * compares the original estimated trip travel time to the observed
    * trip travel time for all trips and
    */
  def endOfEpisodeRewardByTripComparison(
    tripLogs: List[TripLogRow],
    allocationTransform: AllocationTransform = AllocationTransform.default()
  ): IO[List[(String, Reward)]] = {
    val diffs  = tripLogs.map { r => (r.agentId, r.travelTimeDiff.value.toDouble) }
    val result = generateSingleAgentRewards(diffs, allocationTransform)
    IO.fromEither(result)
  }

  def finalObservations(
    tripLogs: List[TripLogRow],
    space: DriverPolicySpace,
    finalBank: Map[String, Karma]
  ): IO[List[Observation]] = {
    tripLogs.traverse { row =>
      for {
        balance <- IO.fromEither(finalBank.getOrError(row.agentId))
        obs <- space.encodeFinalObservation(
          originalTravelTimeEstimate = row.originalTravelTimeEstimate,
          finalTravelTime = row.finalTravelTime,
          finalBankBalance = balance
        )
      } yield Observation.SingleAgentObservation(obs)
    }
  }

  // /**
  //   * compares the travel time of each trip from a reference run. computes
  //   * the fairness of the travel time diff using the user-perceived variant of
  //   * Jain's Fairness Index.
  //   *
  //   * for RL models which are not multi-agent. this assumes that, for each
  //   * (AgentId, Reward) pair, we are logging separate returns to some known
  //   * and shared policy.
  //   *
  //   * here, we only concern ourselves with minimizing unfairness. if a result is
  //   * better than fair (i.e., improves travel time), we report it as a reward of
  //   * zero. otherwise, we report a negative reward in [-1, 0) which captures the
  //   * unfair allocation with respect to the group.
  //   *
  //   * @param config
  //   * @param referenceExperiment
  //   * @return
  //   */
  // def generateEpisodeRewardFromSelfishComparison(
  //   config: MATSimRunConfig,
  //   referenceExperiment: String = "selfish",
  //   agentExperienceFile: String = "agentExperience.csv",
  //   allocationTransform: AllocationTransform = AllocationTransform.default()
  // ): IO[List[(String, Reward)]] = {
  //   val selfishDir  = config.experimentDirectory.getParent.resolve(referenceExperiment)
  //   val selfishFile = selfishDir.resolve(agentExperienceFile).toFile
  //   val optimalFile = config.experimentDirectory.resolve(agentExperienceFile).toFile

  //   val result = for {
  //     s       <- ReadResult.sequence(selfishFile.asCsvReader[AgentExperienceRow](rfc.withHeader).toList)
  //     o       <- ReadResult.sequence(optimalFile.asCsvReader[AgentExperienceRow](rfc.withHeader).toList)
  //     diffs   <- computeDiffs(s, o)
  //     rewards <- generateSingleAgentRewards(diffs, allocationTransform)
  //   } yield rewards
  //   IO.fromEither(result)
  // }

  /**
    * generates a reward for each agent which is computed via the user-perceived fairness
    * equation.
    *
    * @param diffs travel time diff from reference to result, computed as (ref - res). if
      the result is faster than the reference, the value is positive.
      @param transform function used to transform the diffs into "allocations"
      @tparam T some object containing data about the agent that we don't care about
    * @return the agents and their rewards after computing our user fairness metric
    */
  def generateSingleAgentRewards[T](
    diffs: List[(T, Double)],
    transform: AllocationTransform = AllocationTransform.default()
  ): Either[Error, List[(T, Reward)]] = {

    val (agentData, agentDiffs) = diffs.unzip

    // we transform the data based on the provided [[AllocationTransform]] function(s)
    val allocations = transform.applyTransform(agentDiffs)
    JainFairnessMath.userFairness(allocations) match {
      case None =>
        if (allocations.nonEmpty) {
          val warning = s"unusual fairness math result returned None " +
            s"but there are ${allocations.length} allocations"
          logger.warn(warning)
        }
        val zeroRewards = agentData.toList.map { case a => (a, Reward.SingleAgentReward(0.0)) }
        Right(zeroRewards)
      case Some(userFairness) =>
        //
        val rewards = userFairness.map { Reward.SingleAgentReward.apply }
        val result  = agentData.toList.zip(rewards)
        Right(result)
    }
  }

  // /**
  //   * match all agent id / trip combinations between the two files
  //   * and compute the difference in travel time for each match.
  //   *
  //   * these appear in the order sorted by the index of (AgentId, TripNumber), so that
  //   * it is lexicographical by agent id and increasing by trip number, such as:
  //   * > List(("a", 1), ("a", 0), ("b", 2)).sorted
  //   * produces
  //   * > List[(String, Int)] = List(("a", 0), ("a", 1), ("b", 2))
  //   *
  //   * @param selfish the selfish rows
  //   * @param optimal the optimal rows
  //   * @return the diffs (optimal - selfish) or an error
  //   */
  // def computeDiffs(
  //   selfish: List[AgentExperienceRow],
  //   optimal: List[AgentExperienceRow]
  // ): Either[Error, List[(String, Double)]] = {

  //   val sLookup = selfish.map { r => (r.agentId, r) }.toMap
  //   val result = optimal.traverse { oRow =>
  //     val agentId = oRow.agentId
  //     sLookup.get(agentId) match {
  //       case None =>
  //         val msg = s"agent from optimal run not found in selfish run: $agentId"
  //         Left(new Error(msg))
  //       case Some(sRow) =>
  //         val diff = oRow.travelTime - sRow.travelTime
  //         Right((agentId, diff))
  //     }
  //   }

  //   result
  // }

  // def departureTimeFromAgentExperienceRow(row: AgentExperienceRow): Either[Error, Long] =
  //   Try {
  //     val t = LocalTime.parse(row.departureTime)
  //     t.toSecondOfDay.toLong
  //   }.toEither.left.map { t => new Error(s"failure parsing departure time ${row.departureTime}", t) }

  /**
    * rows are listed by agentId and departure time,
    * but the departure times may vary between files when comparing.
    * in order to properly compare rows, we need to generate the trip
    * index for each row (the sequential ordering value for each trip
    * for each agent).
    *
    * @param rows the rows to annotate
    * @param extractAgentId function to extract the agent id from the row
    * @param extractDepartureTime function to extract the departure time from the row
    * @return rows indexed by agent id and trip index, starting from 1.
    */
  def createAgentAndTripIndex[T](extractAgentId: T => String, extractDepartureTime: T => Either[Error, Long])(
    rows: List[T]
  ): Either[Error, List[((String, Int), T)]] = {
    val withTimes = rows.traverse { row =>
      extractDepartureTime(row).map { dTimeSeconds => ((extractAgentId(row), dTimeSeconds), row) }
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

    withIdx.left.map { t => new Error("error parsing local time from agent row") }
  }

}
