package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.config.batch.MATSimBatchConfig
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import pureconfig.ConfigSource

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
          val goodConfsCount: Int = confs.count { case MATSimBatchConfig.Variation(conf, _, _) =>
            conf match {
              case Left(e) =>
                println(e.prettyPrint())
                false
              case Right(_) => true
            }
          }

          println(s"generated $goodConfsCount valid configs for batch with $trials trials per conf -> ${goodConfsCount * trials} total trials")

          if (goodConfsCount == 0) {
            // todo: revert to default conf?
            System.exit(1)
          } else {
            // set up batch logger to report any failures
            val batchLoggerPath: Path =
              confs
                .headOption.map{_.configReaderResult.map{ firstConf =>
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
              (MATSimBatchConfig.Variation(confReaderResult, variationHint, popSize), experimentNumber) <- confs.zipWithIndex
              conf <- confReaderResult
              confWithBatchName = conf.copy(io=conf.io.copy(batchName = batchName))
              trial <- 0 until trials
              scenarioData = MATSimConfig.IO.ScenarioData(
                algorithm = confWithBatchName.algorithm.name,
                variation = MATSimBatchConfig.createVariationName(variationHint),
                trialNumber = trial)
              confWithScenarioData = confWithBatchName.copy(io=confWithBatchName.io.copy(scenarioData = Some{scenarioData}))
              popFileName: String = s"population-$popSize-$trial.xml"
              variationPopFile: File = confWithScenarioData.io.batchLoggingDirectory.resolve(popFileName).toFile
              matsimConfig = confWithScenarioData.copy(io=confWithScenarioData.io.copy(populationFile = variationPopFile))
            } {

              Files.createDirectories(matsimConfig.io.experimentLoggingDirectory)
              val variationOutput: String = variationHint.mkString("\n")
              val variationOutputFile: File = matsimConfig.io.experimentLoggingDirectory.resolve("variation.txt").toFile
              val variationPrintWriter: PrintWriter = new PrintWriter(variationOutputFile.toString)
              variationPrintWriter.write(variationOutput)
              variationPrintWriter.close()

              // test if population has been generated or if we need to create it
              if (variationPopFile.isFile) {

                // population already exists. run experiment
                val experiment: MATSimExperimentRunner = MATSimExperimentRunner(matsimConfig)

                experiment.run() match {
                  case Left(e) =>
                    println(s"trial $trial exit with experiment run failure")
                    batchLogger.write(s"$trial,${e.toString}\n")
                  case Right(e) =>
                    println(s"trial $trial exited normally")
                    batchLogger.write(s"$trial,${matsimConfig.io.experimentDirectory}\n")
                }
              } else {

                // generate population before starting the experiment
                MATSimPopulationRunner.generateUniformPopulation(
                  matsimConfig.io.matsimNetworkFile,
                  variationPopFile,
                  popSize,
                  0.2,
                  seed = Some { experimentNumber }
                ) match {
                  case Left(e) => println(e)
                  case Right(_) =>
                    val experiment: MATSimExperimentRunner = MATSimExperimentRunner(matsimConfig)

                    experiment.run() match {
                      case Left(e) =>
                        println(s"trial $trial exit with experiment run failure")
                        batchLogger.write(s"$trial,${e.toString}\n")
                      case Right(e) =>
                        println(s"trial $trial exited normally")
                        batchLogger.write(s"$trial,${matsimConfig.io.experimentDirectory}\n")
                    }
                }
              }
            }
            // close logger after finishing all batches
            batchLogger.close()
            println("finished.")
          }
      }
    }
  }
)
