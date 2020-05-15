package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._

object MonteCarloTravelTimeLowerBoundsApp
    extends CommandApp(
      name = "monte-carlo-lower-bounds",
      header = "approximate the free-flow average network travel time via monte carlo sampling",
      main = {
        val fileOpt: Opts[Path] =
          Opts.option[Path](long = "file", short = "f", help = "a MATSim network file")

        val nOpt: Opts[Int] =
          Opts.option[Int](long = "n", short = "n", help = "number of samples")

        val seedOpt: Opts[Long] =
          Opts.option[Long](long = "seed", short = "s", help = "random seed value").withDefault(System.currentTimeMillis)

        (fileOpt, nOpt, seedOpt).mapN { (network, n, seed) =>
          {
            val result: Either[String, Double] = MonteCarloTravelTimeLowerBoundsOps.monteCarloTravelTimeLowerBoundSample(network, seed, n)

            result match {
              case Left(e) =>
                println(s"failed due to $e")
                ()
              case Right(result) =>
                println(f"monte carlo travel time estimate after $n samples is $result%.2f")
                ()
            }
          }
        }
      }
    )
