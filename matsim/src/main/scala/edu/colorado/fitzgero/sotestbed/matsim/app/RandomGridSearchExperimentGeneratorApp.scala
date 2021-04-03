package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.PrintWriter
import java.nio.file.Path

import scala.util.Random

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.Configuration
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.Configuration._
import kantan.csv.ops._
import kantan.csv.rfc

object RandomGridSearchExperimentGeneratorApp
    extends CommandApp(
      name = "so-testbed-random-grid-search",
      header = "creates a grid search of random experiments using the Configuration generator classes",
      main = {

        val nOpt: Opts[Int] =
          Opts.option[Int](long = "number", short = "n", help = "the number of scenarios to make")
        val outOpt: Opts[Path] =
          Opts.option[Path](long = "out", short = "o", help = "the output directory")
        val seedOpt: Opts[Option[Long]] = {
          Opts.option[Long](long = "seed", short = "s", help = "random seed value").orNone
        }

        (nOpt, outOpt, seedOpt)
          .mapN {
            (n, out, seed) =>
              {
                if (!out.toFile.isDirectory) {
                  println(s"creating out directory $out")
                  out.toFile.mkdirs()
                }

                val random = seed match {
                  case Some(value) => new Random(value)
                  case None        => Random
                }

                val confs: Seq[Configuration] = for {
                  i    <- 0 until n
                  conf <- Configuration.generate(random, Some(i.toString))
                } yield {
                  val pw = new PrintWriter(out.resolve(conf.filename).toFile)
                  pw.write(conf.toHocon)
                  pw.close()
                  conf
                }

                out.resolve("configuration_table.csv").writeCsv(confs, rfc.withHeader)
                println(s"$n generated configurations written to $out")
              }
          }
      }
    )
