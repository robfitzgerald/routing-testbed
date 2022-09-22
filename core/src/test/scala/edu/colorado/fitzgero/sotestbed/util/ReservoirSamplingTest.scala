package edu.colorado.fitzgero.sotestbed.util

import scala.util.Random
import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class ReservoirSamplingTest extends SoTestBedBaseTest {
  "Algorithm A-ExpJ" when {
    "called with some sample data" should {
      "pick generally from the higher-valued population" in {
        val rng       = new Random(2)
        val maxWeight = 100.0
        val samples   = 1000
        val k         = 10
        // 1% chance it's a zero, then another 1% chance it's in [85, 100]
        // otherwise we pick uniformly in [0, 90]
        def computeWeight: Double = {
          if (rng.nextDouble <= 0.01) 0.0
          else if (rng.nextDouble < 0.01) rng.between(0.85, 0.99) * maxWeight
          else rng.between(0.0, 0.9) * maxWeight
        }
        val population = for {
          id <- (0 until samples).toList
        } yield (id.toString, computeWeight)
        val (sample, remaining) = ReservoirSampling.aExpJ(rng, population, k)
        sample.size should equal(k)
        remaining.size should equal(samples - k)

        // visual inspection of the sample
        sample.foreach(println)
        val hist = remaining.foldLeft(Array.fill(10)(0)) {
          case (bins, (_, w)) =>
            val bin = (w / 10.0).toInt
            bins.updated(bin, bins(bin) + 1)
        }
        println((0 until 10).map { n => if (n == 0) " 0" else f"${n * 10}" }.mkString("   ", "   |    ", "  "))
        println(hist.mkString("  ", "   |   ", "  "))
      }
    }
  }
}
