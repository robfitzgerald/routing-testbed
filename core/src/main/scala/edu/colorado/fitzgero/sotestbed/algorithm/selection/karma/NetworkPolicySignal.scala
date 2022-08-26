package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import org.apache.commons.math.distribution.{BetaDistribution, BetaDistributionImpl}
import com.typesafe.scalalogging.LazyLogging

sealed trait NetworkPolicySignal

object NetworkPolicySignal extends LazyLogging {

  case object UserOptimal extends NetworkPolicySignal

  /**
    * a system-optimal policy instance where a random system-optimal strategy is
    * applied to some percentage of the drivers
    * @param thresholdPercent the percentage of drivers to assign a random so route
    */
  case class ThresholdSampling(thresholdPercent: Double, random: Random) extends NetworkPolicySignal {
    override def toString: String = f"ThresholdSampling(percent=${thresholdPercent * 100}%.2f%%)"
  }

  object ThresholdSampling {

    def build(percent: Double, seed: Long): Either[Error, ThresholdSampling] = {
      if (percent < 0.0 || percent > 1.0) Left(new Error(s"percent must be in [0, 1], got $percent"))
      else Right(ThresholdSampling(percent, new Random(seed)))
    }
  }

  /**
    * a system-optimal policy instance where some percentage of drivers may take
    * their user-optimal path, and the remaining must take a route that is some degree system-optimal
    *
    * @param thresholdPercent the percentage of drivers to assign a random so route
    * @param bernoulliPercent percentage of unfair the remaining routes should be
    */
  case class BernoulliDistributionSampling(thresholdPercent: Double, bernoulliPercent: Double)
      extends NetworkPolicySignal

  object BernoulliDistributionSampling {

    def build(pct: Double, deg: Double): Either[Error, BernoulliDistributionSampling] = {
      if (pct < 0.0 || pct > 1.0) Left(new Error(s"percent must be in [0, 1], got $pct"))
      else if (deg < 0.0 || deg > 1.0) Left(new Error(s"degreePct must be in [0, 1], got $deg"))
      else {
        Right(BernoulliDistributionSampling(pct, deg))
      }
    }
  }

  /**
    * a sampling-based approach. given parameters for a beta distribution, sample
    * the so degree percentage value for all drivers from this distribution.
    *
    * @param dist the beta distribution to sample from
    */
  case class BetaDistributionSampling(dist: BetaDistributionImpl) extends NetworkPolicySignal

  object BetaDistributionSampling {

    final case class Sample(value: Double) extends AnyVal

    /**
      * constructor for a BetaDistSampling object which takes the two parameters
      * alpha and beta of a beta distribution to construct the underlying distribution
      * @param alpha alpha parameter
      * @param beta beta parameter
      * @return either a beta distribution policy, or, an error
      */
    def apply(alpha: Double, beta: Double): Either[Error, BetaDistributionSampling] = {
      if (alpha < 0.0) Left(new Error(s"alpha must be positive, got $alpha"))
      else if (beta < 0.0) Left(new Error(s"beta must be positive, got $beta"))
      else {
        val dist = new BetaDistributionImpl(alpha, beta)
        Right(BetaDistributionSampling(dist))
      }
    }
  }

  /**
    * gets the coefficients for this network signal as a comma-delimited string for logging
    * @return
    */
  def getLogHeader(networkPolicy: NetworkPolicyConfig): String = networkPolicy match {
    case NetworkPolicyConfig.UserOptimal                        => ""
    case _: NetworkPolicyConfig.RandomPolicy                    => ""
    case _: NetworkPolicyConfig.CongestionProportionalThreshold => "p"
    case _: NetworkPolicyConfig.ScaledProportionalThreshold     => "p"
  }

  implicit class NetworkPolicySignalOps(sig: NetworkPolicySignal) {

    /**
      * gets the coefficients for this network signal as a comma-delimited string for logging
      * @return
      */
    def getLogData: String = sig match {
      case NetworkPolicySignal.UserOptimal => ""
      case bernie: ThresholdSampling       => s"${bernie.thresholdPercent}"
      // not yet implemented as [[NetworkPolicyConfig]] types:
      case bernie: BernoulliDistributionSampling => s"${bernie.thresholdPercent},${bernie.bernoulliPercent}"
      case beta: BetaDistributionSampling        => s"${beta.dist.getAlpha},${beta.dist.getBeta}"
    }

    def assign(bids: List[Bid], alts: Map[Request, List[Path]]): List[(Bid, Int, Path)] =
      if (bids.lengthCompare(1) == 0) {
        pickPaths(bids, alts, uoPathSelection)
      } else {
        sig match {

          case UserOptimal =>
            pickPaths(bids, alts, uoPathSelection)

//            def weighted_sample_without_replacement(population, weights, k, rng=random):
          //    v = [rng.random() ** (1 / w) for w in weights]
          //    order = sorted(range(len(population)), key=lambda i: v[i])
          //    return [population[i] for i in order[-k:]]

          case sop: ThresholdSampling =>
            val bidsLowestToHighest = bids.sortBy { _.value }
            val numAgentsSo         = percentToDiscreteRange(sop.thresholdPercent, alts.size)
            val bidsSo              = bidsLowestToHighest.take(numAgentsSo)
            val bidsUo              = bidsLowestToHighest.drop(numAgentsSo)

            // pick a random path for the redirected agents from range [1, n)
            // as path 0 is the true shortest path
            val routesRedirected = pickPaths(
              bidsSo,
              alts,
              (bid, paths) => {
                if (paths.length == 1) {
                  logger.warn(s"bid only has 1 path, must choose it: $bid")
                  (bid, 0, paths.head)
                } else {
                  val selectedPathIdx = sop.random.nextInt(paths.length - 1) + 1
                  val path            = paths(selectedPathIdx)
                  (bid, selectedPathIdx, path)

                }
              }
            )
            val routesShortestPath = pickPaths(bidsUo, alts, uoPathSelection)
            val result             = routesRedirected ++ routesShortestPath
            result

          case sopad: BernoulliDistributionSampling =>
            val bidsLowestToHighest = bids.sortBy { _.value }
            val numAgentsSo         = percentToDiscreteRange(sopad.thresholdPercent, alts.size)
            val bidsSo              = bidsLowestToHighest.take(numAgentsSo)
            val bidsUo              = bidsLowestToHighest.drop(numAgentsSo)

            // pick a path of some degree of optimal for the SO agents
            val routesRedirected = pickPaths(
              bidsSo,
              alts,
              (bid, paths) => {
                val selectedPathIdx = percentToDiscreteRange(sopad.bernoulliPercent, paths.length)
                val path            = paths(selectedPathIdx)
                (bid, selectedPathIdx, path)
              }
            )
            val routesShortestPath = pickPaths(bidsUo, alts, uoPathSelection)
            val result             = routesRedirected ++ routesShortestPath
            result

          case BetaDistributionSampling(dist) =>
            val samples                = for { _ <- bids.indices } yield dist.sample()
            val samplesLowestToHighest = samples.sorted.toList
            val bidsHighestToLowest    = bids.sortBy { -_.value }
            val lookup                 = bidsHighestToLowest.zip(samplesLowestToHighest).toMap
            val routes = pickPaths(
              bids,
              alts,
              (bid, paths) => {
                lookup.get(bid) match {
                  case None => throw new IllegalStateException()
                  case Some(sample) =>
                    val selectedPathIdx = percentToDiscreteRange(sample, paths.length)
                    val path            = paths(selectedPathIdx)
                    (bid, selectedPathIdx, path)
                }
              }
            )
            routes
        }
      }
  }

  /**
    * maps some percentage value into a range of discrete values like [0, 1] => [0, max)
    * @param pct percentage to map
    * @param max something like a collection size or count of classes in a classifier
    * @return the pct value as a discrete value
    */
  def percentToDiscreteRange(pct: Double, max: Int): Int = {
    val result = math.max(0.0, math.min(max, max * pct)).toInt
    result
  }

  /**
    * combinator that applies a function which selects paths based on a bid
    * @param bids bids to apply to path selection
    * @param alts the original requests and their alternative path sets
    * @param fn function to pick paths based on bid
    * @return the selected path, it's index, along with the bid
    */
  def pickPaths(
    bids: List[Bid],
    alts: Map[Request, List[Path]],
    fn: (Bid, List[Path]) => (Bid, Int, Path)
  ): List[(Bid, Int, Path)] = {
    val result = for {
      bid   <- bids
      paths <- alts.get(bid.request)
    } yield fn(bid, paths)
    result
  }

  def uoPathSelection: (Bid, List[Path]) => (Bid, Int, Path) = (bid, paths) => (bid, 0, paths.head)
}
