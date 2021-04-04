package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.PrintWriter
import java.nio.file.Path

import scala.util.Random

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.generator.{Configuration, GeneratorAppOps, Scenario}
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
        val partitionsOps: Opts[Int] =
          Opts.option[Int](long = "partitions", short = "p", help = "number of sub-directories to make")
        val seedOpt: Opts[Option[Long]] = {
          Opts.option[Long](long = "seed", short = "s", help = "random seed value").orNone
        }

        (nOpt, outOpt, partitionsOps, seedOpt)
          .mapN {
            (n, out, partitions, seed) =>
              {
                // random seed for generating each configuration
                val random = seed match {
                  case Some(value) => new Random(value)
                  case None        => Random
                }

                // create _n_ random configurations
                val confs: Seq[Configuration] = for {
                  i    <- 0 until n
                  conf <- Configuration.generate(random, Some(i.toString))
                } yield conf

                // create the output base directory
                if (out.toFile.isDirectory) {
                  throw new IllegalArgumentException(s"output directory $out already exists")
                } else {
                  out.toFile.mkdirs()
                  println(s"created out directory $out")
                }

                // write summary table
                out.resolve("configuration_table.csv").writeCsv(confs, rfc.withHeader)
                println(s"configurations table written to $out")

                // partition configurations into _partitions_ splits
                val partitionSize = math.ceil(n.toDouble / partitions).toInt * 4 // |{Selfish,Base,Rand,MCTS}| == 4
                val partitioned   = confs.sliding(partitionSize, partitionSize).toArray
                println(s"created $partitions partitions of $n configurations with partition size $partitionSize")

                for {
                  partitionId <- partitioned.indices
                } yield {
                  val thisPartition = partitioned(partitionId)

                  val partitionOut = out.resolve(partitionId.toString)
                  partitionOut.toFile.mkdirs()
                  println(s"created out directory $partitionOut")

                  // write all dependencies to this partition folder
                  val program = for {
                    _ <- GeneratorAppOps.copyDependencies(partitionOut, Scenario.All)
                  } yield {
                    println(s"$n generated configurations written to $out")
                  }
                  program.unsafeRunSync

                  // write all configs for this partition to this partition folder
                  for {
                    conf <- thisPartition
                  } {
                    val pw = new PrintWriter(partitionOut.resolve(conf.filename).toFile)
                    pw.write(conf.toHocon)
                    pw.close()
                  }

                  // write bash script for this partition
                  val bashScriptText = GeneratorAppOps.batchShellScript(thisPartition)
                  val bashFile       = partitionOut.resolve("batch.sh").toFile
                  val bashPW         = new PrintWriter(bashFile)
                  bashPW.write(bashScriptText)
                  bashPW.close()
                  println(s"partition runner script written to $bashFile")
                }
              }
          }
      }
    )
