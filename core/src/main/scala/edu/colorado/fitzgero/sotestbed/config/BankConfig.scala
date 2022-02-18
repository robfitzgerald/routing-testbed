package edu.colorado.fitzgero.sotestbed.config

import scala.util.Random

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma

sealed trait BankConfig

object BankConfig {

  case class Fixed(value: Karma, max: Karma) extends BankConfig

  case class Uniform(max: Karma, seed: Long = 0L) extends BankConfig

  case class Median(max: Karma) extends BankConfig

//  case class Tabular(file: File, nameCol: String, balanceCol: String) extends BankConfig

  implicit class BankConfigExtensionMethods(bankConfig: BankConfig) {

    def build(agentsUnderControl: Set[String]): Map[String, Karma] = bankConfig match {
      case Fixed(value, _) =>
        agentsUnderControl.map { a => (a, value) }.toMap
      case Uniform(max, seed) =>
        val rnd = new Random(seed)
        agentsUnderControl.map { a => (a, Karma(rnd.nextDouble * max.value)) }.toMap
      case Median(max) =>
        val median = Karma(max.value / 2)
        agentsUnderControl.map { a => (a, median) }.toMap
    }

    def max: Karma = bankConfig match {
      case Fixed(_, max)   => max
      case Uniform(max, _) => max
      case Median(max)     => max
    }
  }
}
