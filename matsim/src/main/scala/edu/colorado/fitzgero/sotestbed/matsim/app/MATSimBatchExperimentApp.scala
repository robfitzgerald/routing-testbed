package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.batch.MATSimBatchConfig
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig

object MATSimBatchExperimentApp extends CommandApp(
  name = "so-testbed-batch-experiment-app",
  header = "runs a set of trials over the so-testbed",
  main = {

    val batchNameOpt: Opts[String] =
      Opts.option[String](long = "batchName", short = "n", help = "the top-level directory for this batch output")

    val batchConfigOpt: Opts[String] =
      Opts.option[String](long = "batchConfig", short = "b", help = "a file with arrays at all paths, used to generate variations")

    val scenarioConfigDirectoryOpt: Opts[String] =
      Opts.option[String](
        long = "scenarioConfigDirectory",
        short = "s",
        help = "a directory with config files (with default parameters) for each named algorithm variant")

    val trialsPerConfigOpt: Opts[Int] =
      Opts.option[Int]("trials", short = "t", help = "number of trials per variation").withDefault(1)

    (batchNameOpt, batchConfigOpt, scenarioConfigDirectoryOpt, trialsPerConfigOpt)
      .mapN { (batchName, batchConfig, scenarioDefaultConfPath, trials) =>

      MATSimBatchConfig.readBatchConfig(new File(batchConfig), Paths.get(scenarioDefaultConfPath)) match {
        case Left(e) =>
          println("failed to read batch config")
          println(e)
          System.exit(1)

        case Right(confs) =>

          // write any batch config generation errors to the console; count confs that are ok
          val goodConfsCount: Int = confs.count { case (conf, _) =>
            conf match {
              case Left(e) =>
                println(e.prettyPrint())
                false
              case Right(_) => true
            }
          }

          println(s"generated $goodConfsCount valid configs for batch with $trials trials per conf -> ${goodConfsCount * trials} total trials")

          // set up batch logger to report any failures
          val batchLoggerPath: Path =
            confs
              .headOption.map{_._1.map{ firstConf =>
                // screwy way to inject the batch name here, so we can construct the correct batchLoggingDirectory
                firstConf.copy(io=firstConf.io.copy(batchName = batchName)).io.batchLoggingDirectory
              }
              .getOrElse(Paths.get("/tmp"))}
              .getOrElse(Paths.get("/tmp"))

          if (Files.exists(batchLoggerPath)) {
            println(s"path $batchLoggerPath already exists. please re-run with a new batchName")
            System.exit(1)
          } else {
            Files.createDirectories(batchLoggerPath)
          }

          val batchLoggerFile: File = batchLoggerPath.resolve("batch_log.csv").toFile
          val batchLogger: PrintWriter = new PrintWriter(batchLoggerFile)
          val batchHeader: String = "trial,message\n"
          batchLogger.write(batchHeader)

          println(s"created batch logging file at $batchLoggerFile")

          // load and run each config variation
          for {
            ((confReaderResult, variationHint), variationNumber) <- confs.zipWithIndex
            conf <- confReaderResult
            confWithBatchName = conf.copy(io=conf.io.copy(batchName = batchName))
            trial <- (0 until trials).map{t => (variationNumber * trials) + t}
            scenarioData = MATSimConfig.IO.ScenarioData(
              algorithm = confWithBatchName.algorithm.name,
              variation = MATSimBatchConfig.createVariationName(variationHint),
              trialNumber = trial)
            confWithScenario = confWithBatchName.copy(io=confWithBatchName.io.copy(scenarioData = Some{scenarioData}))
          } {

            val experiment: MATSimExperimentRunner = MATSimExperimentRunner(confWithScenario)

            experiment.run() match {
              case Left(e) =>
                println(s"trial $trial exit with experiment run failure")
                batchLogger.write(s"$trial,${e.toString}\n")
              case Right(e) =>
                println(s"trial $trial exited normally")
                batchLogger.write(s"$trial,${confWithScenario.io.experimentDirectory}\n")
            }
          }
          // close logger after finishing all batches
          batchLogger.close()
          println("finished.")
      }
    }
  }
)
