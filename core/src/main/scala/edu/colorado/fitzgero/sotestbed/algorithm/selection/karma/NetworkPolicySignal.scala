package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import scala.math.Numeric.Implicits.infixNumericOps
import scala.util.Random

import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import org.apache.commons.math.distribution.{BetaDistribution, BetaDistributionImpl}

sealed trait NetworkPolicySignal

object NetworkPolicySignal {

  case object UserOptimal extends NetworkPolicySignal

  /**
    * a system-optimal policy instance where a random system-optimal strategy is
    * applied to some percentage of the drivers
    * @param percent the percentage of drivers to assign a random so route
    */
  case class BernoulliDistributionSampling(percent: Double, random: Random, congestion: Double)
      extends NetworkPolicySignal

  object BernoulliDistributionSampling {

    def build(percent: Double, seed: Long, congestion: Double): Either[Error, BernoulliDistributionSampling] = {
      if (percent < 0.0 || percent > 1.0) Left(new Error(s"percent must be in [0, 1], got $percent"))
      else Right(BernoulliDistributionSampling(percent, new Random(seed), congestion))
    }
  }

  /**
    * a system-optimal policy instance where some percentage of drivers may take
    * their user-optimal path, and the remaining must take a route that is some degree system-optimal
    *
    * @param percent the percentage of drivers to assign a random so route
    * @param degreePct percentage of unfair the remaining routes should be
    */
  case class BernoulliDistSampleWithSODegree(percent: Double, degreePct: Double, congestion: Double)
      extends NetworkPolicySignal

  object BernoulliDistSampleWithSODegree {

    def build(pct: Double, deg: Double, congestion: Double): Either[Error, BernoulliDistSampleWithSODegree] = {
      if (pct < 0.0 || pct > 1.0) Left(new Error(s"percent must be in [0, 1], got $pct"))
      else if (deg < 0.0 || deg > 1.0) Left(new Error(s"degreePct must be in [0, 1], got $deg"))
      else {
        Right(BernoulliDistSampleWithSODegree(pct, deg, congestion))
      }
    }
  }

  /**
    * a sampling-based approach. given parameters for a beta distribution, sample
    * the so degree percentage value for all drivers from this distribution.
    *
    * @param dist the beta distribution to sample from
    */
  case class BetaDistributionSampling(dist: BetaDistributionImpl, congestion: Double) extends NetworkPolicySignal

  object BetaDistributionSampling {

    final case class Sample(value: Double) extends AnyVal

    /**
      * constructor for a BetaDistSampling object which takes the two parameters
      * alpha and beta of a beta distribution to construct the underlying distribution
      * @param alpha alpha parameter
      * @param beta beta parameter
      * @return either a beta distribution policy, or, an error
      */
    def apply(alpha: Double, beta: Double, congestion: Double): Either[Error, BetaDistributionSampling] = {
      if (alpha < 0.0) Left(new Error(s"alpha must be positive, got $alpha"))
      else if (beta < 0.0) Left(new Error(s"beta must be positive, got $beta"))
      else {
        val dist = new BetaDistributionImpl(alpha, beta)
        Right(BetaDistributionSampling(dist, congestion))
      }
    }
  }

  implicit class NetworkPolicySignalOps(sig: NetworkPolicySignal) {

    def congestion: Option[Double] = sig match {
      case UserOptimal                              => None
      case bds: BernoulliDistributionSampling       => Some(bds.congestion)
      case bdswsod: BernoulliDistSampleWithSODegree => Some(bdswsod.congestion)
      case bds: BetaDistributionSampling            => Some(bds.congestion)
    }

    // corresponds with [[NetworkPolicy.signalHeader]]
    def getCoefs: String = sig match {
      case NetworkPolicySignal.UserOptimal         => ","
      case bernie: BernoulliDistributionSampling   => s"${bernie.percent},"
      case bernie: BernoulliDistSampleWithSODegree => s"${bernie.percent},${bernie.degreePct}"
      case beta: BetaDistributionSampling          => s"${beta.dist.getAlpha},${beta.dist.getBeta}"
    }

    def assign(bids: List[Bid], alts: Map[Request, List[Path]]): List[(Bid, Int, Path)] =
      sig match {
        case UserOptimal =>
          pickPaths(bids, alts, uoPathSelection)
        case sop: BernoulliDistributionSampling =>
          val bidsLowestToHighest = bids.sortBy { _.value }
          val numAgentsSo         = percentToDiscreteRange(sop.percent, alts.size)
          val bidsSo              = bidsLowestToHighest.take(numAgentsSo)
          val bidsUo              = bidsLowestToHighest.drop(numAgentsSo)
          // pick a random path for the SO agents
          val routesSo = pickPaths(bidsSo, alts, (bid, paths) => {
            val selectedPathIdx = sop.random.nextInt(paths.length)
            val path            = paths(selectedPathIdx)
            (bid, selectedPathIdx, path)
          })
          val routesUo = pickPaths(bidsUo, alts, uoPathSelection)
          routesSo ++ routesUo

        case sopad: BernoulliDistSampleWithSODegree =>
          val bidsLowestToHighest = bids.sortBy { _.value }
          val numAgentsSo         = percentToDiscreteRange(sopad.percent, alts.size)
          val bidsSo              = bidsLowestToHighest.take(numAgentsSo)
          val bidsUo              = bidsLowestToHighest.drop(numAgentsSo)
          // pick a path of some degree of optimal for the SO agents
          val routesSo = pickPaths(
            bidsSo,
            alts,
            (bid, paths) => {
              val selectedPathIdx = percentToDiscreteRange(sopad.degreePct, paths.length)
              val path            = paths(selectedPathIdx)
              (bid, selectedPathIdx, path)
            }
          )
          val routesUo = pickPaths(bidsUo, alts, uoPathSelection)
          routesSo ++ routesUo

        case BetaDistributionSampling(dist, _) =>
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

  /**
    * maps some percentage value into a range of discrete values like [0, 1] => [0, max)
    * @param pct percentage to map
    * @param max something like a collection size or count of classes in a classifier
    * @return the pct value as a discrete value
    */
  def percentToDiscreteRange(pct: Double, max: Int): Int = math.min(max - 1, max * pct).toInt

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
