package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.JainFairnessMath.CovType.Biased
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.JainFairnessMath.CovType.Unbiased

object JainFairnessMath {

  // the COV can be estimated via stdev/mean but this is a biased estimation
  // for normally-distributed data, use the unbiased formula. see:
  // [[https://en.wikipedia.org/wiki/Coefficient_of_variation#Estimation]]
  sealed trait CovType

  object CovType {
    case object Biased   extends CovType
    case object Unbiased extends CovType
  }

  /**
    * formula 1: compute the global fairness for a sequence of allocations
    * (the original Jain Fairness Index)
    *
    * @param xs allocations for each user
    * @return the fairness of the allocations
    */
  def fairness(xs: Seq[Double], covType: CovType = CovType.Biased): Option[Double] = {
    cov(xs, covType).flatMap {
      case c if c == -1.0 => None
      case c =>
        val fairness = 1.0 / (1.0 + math.pow(c, 2.0))
        Some(fairness)
    }
  }

  /**
    * corollary 1: find the user-perceived fairness for each allocation
    * in the case that the fairness is uniformly fair to all,
    *
    * @param xs allocations for each user
    * @return the fairness perceived by each allocation
    */
  def userFairness(xs: Seq[Double]): Option[Seq[Double]] =
    fairAllocationMark(xs).flatMap {
      case fam if fam == 0.0 => Some(xs.map { _ => 1.0 })
      case fam =>
        val userFairness = xs.map { _ / fam }
        Some(userFairness)
    }

  /**
    * formula 2: re-write of original Jain Index from the user-perceived fairness values
    * (section 6 of the original Jain Index paper)
    *
    * @param xs allocations for each user
    * @return the fairness of the allocations
    */
  def userFairnessAggregate(xs: Seq[Double]): Option[Double] = userFairness(xs).map { _.sum / xs.length }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val (acc, cnt) = xs.foldLeft((0.0, 0)) { case ((a, n), v) => (a + v, n + 1) }
      val µ          = acc / cnt
      Some(µ)
    }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def variance(xs: Seq[Double], µ: Double): Option[Double] = {
    val vars = xs.map(x => Math.pow(x - µ, 2))
    mean(vars)
  }

  def stdev(xs: Seq[Double]): Option[Double] =
    variance(xs).map {
      case v if v == 0.0 => 0.0
      case v             => math.sqrt(v)
    }

  def stdev(xs: Seq[Double], µ: Double): Option[Double] =
    variance(xs, µ).map {
      case v if v == 0.0 => 0.0
      case v             => math.sqrt(v)
    }

  /**
    * find the coefficient of variation for a vector.
    * only defined for distributions whose mean is
    * positively-valued; but, in this application, we
    * want to return zero if either of those are the case.
    *
    * @param xs the vector to find the cov for
      @return the cov if it defined for this vector. if the
      mean is zero or the stdev is zero, return 1.0.
    */
  def cov(xs: Seq[Double], covType: CovType): Option[Double] = {
    val c_v_hat = for {
      µ <- mean(xs)
      o <- stdev(xs, µ)
    } yield if (µ > 0.0) o / µ else 0.0
    covType match {
      case Biased =>
        c_v_hat
      case Unbiased =>
        val n = xs.length
        c_v_hat.map { cvhat =>
          val coef = (1.0 + (1.0 / (4.0 * n)))
          coef * cvhat
        }
    }
  }

  def fairAllocationMark(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else {
      xs.sum match {
        case 0.0 => Some(0.0)
        case b_1 =>
          val b_2 = xs.map { x => math.pow(x, 2) }.sum
          val fam = b_2 / b_1
          Some(fam)
      }
    }

}
