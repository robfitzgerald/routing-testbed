package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.{File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import scala.util.Try

import cats.data.Writer
import cats.implicits._

import com.monovore.decline._
import com.typesafe.config.{Config, ConfigFactory}
import edu.colorado.fitzgero.sotestbed.matsim.config.batch.MATSimBatchConfig
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig
import pureconfig.ConfigSource

object MATSimBatchExperimentApp
    extends CommandApp(
      name = "so-testbed-batch-experiment-app",
      header = "runs a set of trials over the so-testbed",
      main = {

        val batchNameOpt: Opts[String] =
          Opts.option[String](long = "batchName", short = "n", help = "the top-level directory for this batch output")

        val batchConfigOpt: Opts[String] =
          Opts.option[String](long = "batchConfig", short = "b", help = "a file with arrays at all paths, used to generate variations")

        val scenarioConfigDirectoryOpt: Opts[String] =
          Opts.option[String](long = "scenarioConfigDirectory",
                              short = "s",
                              help = "a directory with config files (with default parameters) for each named algorithm variant")

        val trialsPerConfigOpt: Opts[Int] =
          Opts.option[Int]("trials", short = "t", help = "number of trials per variation").withDefault(1)

        val batchSeedOps: Opts[Long] =
          Opts.option[Long](long = "seed", help = "a seed value to offset how populations are generated ").withDefault(0)

        (batchNameOpt, batchConfigOpt, scenarioConfigDirectoryOpt, trialsPerConfigOpt, batchSeedOps)
          .mapN {
            (batchName, batchConfig, scenarioDefaultConfPath, trials, batchSeed) =>
              MATSimBatchConfig.readBatchConfig(new File(batchConfig), Paths.get(scenarioDefaultConfPath)) match {
                case Left(e) =>
                  println("failed to read batch config")
                  println(e)
                  System.exit(1)

                case Right(confs) =>
                  // write any batch config generation errors to the console; count confs that are ok
                  val goodConfsCount: Int = confs.count {
                    case MATSimBatchConfig.Variation(_, conf, _, _) =>
                      conf match {
                        case Left(e) =>
                          println(e.prettyPrint())
                          false
                        case Right(_) => true
                      }
                  }

                  println(
                    s"generated $goodConfsCount valid configs for batch with $trials trials per conf -> ${goodConfsCount * trials} total trials")

                  if (goodConfsCount == 0) {
                    // todo: revert to default conf?
                    System.exit(1)
                  } else {
                    // set up batch logger to report any failures
                    val batchLoggerPath: Path =
                      confs.headOption
                        .map {
                          _.configReaderResult
                            .map { firstConf =>
                              // screwy way to inject the batch name here, so we can construct the correct batchLoggingDirectory
                              firstConf.copy(io = firstConf.io.copy(batchName = batchName)).io.batchLoggingDirectory
                            }
                            .getOrElse(Paths.get("/tmp"))
                        }
                        .getOrElse(Paths.get("/tmp"))

                    if (Files.exists(batchLoggerPath)) {
                      println(s"path $batchLoggerPath already exists. will only generate experiments that do not overwrite previous ones")
                      //              System.exit(1)
                    } else {
                      Files.createDirectories(batchLoggerPath)
                    }

                    val batchLoggerFile: Path = batchLoggerPath.resolve("batch_log.csv")

                    val batchLogger: PrintWriter = if (!Files.exists(batchLoggerFile)) {
                      val batchLogger: PrintWriter = new PrintWriter(batchLoggerFile.toFile)
                      val batchHeader: String      = "trial,message\n"
                      batchLogger.write(batchHeader)
                      println(s"created batch logging file at $batchLoggerFile")
                      batchLogger
                    } else {
                      val APPEND_MODE: Boolean = true
                      new PrintWriter(new FileOutputStream(batchLoggerFile.toFile, APPEND_MODE))
                    }

                    val batchOverviewFile: Path = batchLoggerPath.resolve("result.csv")
                    if (!Files.exists(batchOverviewFile)) {
                      val batchOverview: PrintWriter = new PrintWriter(batchOverviewFile.toFile)
                      val paramsHeader: String = confs.headOption match {
                        case None    => ""
                        case Some(c) => c.variationHint.keys.toList.sorted.mkString(",")
                      }
                      val batchOverviewHeader: String = s"$paramsHeader,avgTTSecs,avgDistMeters,avgSpeedMph\n"
                      batchOverview.write(batchOverviewHeader)
                      batchOverview.close()
                      println(s"created batch overview file at $batchOverviewFile")
                    }

                    //////////////////////////////////////////////////
                    // let's get the selfish experiments done first //
                    //////////////////////////////////////////////////
                    for {
                      MATSimBatchConfig.Variation(config, confReaderResult, variationHint, popSize) <- confs
                      conf                                                                          <- confReaderResult

                      confWithBatchName = conf.copy(io = conf.io.copy(batchName = batchName))
                      trial <- 0 until trials
                      scenarioData = MATSimConfig.IO.ScenarioData(
                        algorithm = confWithBatchName.algorithm.name,
                        variationName = popSize.toString,
                        trialNumber = trial,
                        headerColumnOrder = variationHint.keys.toList.sorted,
                        scenarioParameters = variationHint
                      )
                      confWithScenarioData = confWithBatchName.copy(io = confWithBatchName.io.copy(scenarioData = Some {
                        scenarioData
                      }))
                      popFileName: String    = s"population-$batchName-$popSize-$trial.xml"
                      variationPopFile: File = confWithScenarioData.io.batchLoggingDirectory.resolve(popFileName).toFile
                      matsimConfig           = confWithScenarioData.copy(io = confWithScenarioData.io.copy(populationFile = variationPopFile))
                    } {
                      if (conf.algorithm.name != "selfish") {
                        // NOOP - only running selfish exp. here
                      } else if (Files.isDirectory(matsimConfig.io.experimentDirectory)) {
                        // don't overwrite if already exists
                        println(s"selfish trial $trial already exists. skipping.")
                        batchLogger.write(s"$trial,run already exists - skipping\n")
                      } else {

                        Files.createDirectories(matsimConfig.io.experimentLoggingDirectory)

                        if (!variationPopFile.isFile) {

                          // generate the population
                          MATSimPopulationRunner.generateUniformPopulation(
                            matsimNetworkFile = matsimConfig.io.matsimNetworkFile,
                            popFileDestination = variationPopFile,
                            popSize = popSize,
                            adoptionRate = matsimConfig.routing.adoptionRate,
                            workActivityMinTime = matsimConfig.population.workActivityMinTime,
                            workActivityMaxTime = matsimConfig.population.workActivityMaxTime,
                            workDurationHours = matsimConfig.population.workDurationHours,
                            seed = Some {
                              trial + batchSeed
                            }
                          ) match {
                            case Left(e) =>
                              println(s"population generation for population-$trial exited with fatal error $e")
                              batchLogger.write(s"$trial,${e.toString}\n")
                            case Right(_) =>
                              ()
                          }

                          // run experiment
                          val experiment: MATSimExperimentRunner = MATSimExperimentRunner(matsimConfig, trial + batchSeed, Some { scenarioData })

                          Try {
                            experiment.run()
                          }.toEither match {
                            case Left(e) =>
                              println(s"trial $trial exit with fatal error $e")
                              batchLogger.write(s"$trial,${e.toString}\n")
                            case Right(result) =>
                              result match {
                                case Left(e) =>
                                  println(s"trial $trial exit with handled error")
                                  batchLogger.write(s"$trial,${e.toString}\n")
                                case Right(_) =>
                                  println(s"trial $trial exited normally")
                                  batchLogger.write(s"$trial,${matsimConfig.io.experimentDirectory}\n")
                              }
                          }
                        }
                      }
                    }

                    /////////////////////////////////////////
                    // ok let's run each so experiment now //
                    /////////////////////////////////////////
                    for {
                      (MATSimBatchConfig.Variation(config, confReaderResult, variationHint, popSize), experimentNumber) <- confs.zipWithIndex
                      conf                                                                                              <- confReaderResult
                      confWithBatchName = conf.copy(io = conf.io.copy(batchName = batchName))
                      trial <- 0 until trials
                      seed          = trial + batchSeed
                      variationName = MATSimBatchConfig.createVariationName(variationHint)
                      scenarioData = MATSimConfig.IO.ScenarioData(
                        algorithm = confWithBatchName.algorithm.name,
                        variationName = variationName,
                        trialNumber = trial,
                        headerColumnOrder = variationHint.keys.toList.sorted,
                        scenarioParameters = variationHint
                      )
                      confWithScenarioData = confWithBatchName.copy(io = confWithBatchName.io.copy(scenarioData = Some {
                        scenarioData
                      }))
                      popFileName: String    = s"population-$batchName-$popSize-$trial.xml"
                      variationPopFile: File = confWithScenarioData.io.batchLoggingDirectory.resolve(popFileName).toFile
                      matsimConfig           = confWithScenarioData.copy(io = confWithScenarioData.io.copy(populationFile = variationPopFile))
                    } {
                      if (conf.algorithm.name == "selfish") {
                        // NOOP - running SO experiments here
                      } else if (Files.isDirectory(matsimConfig.io.experimentDirectory)) {
                        // don't overwrite if already exists
                        println(s"optimal scenario $variationName run already exists. skipping.")
                        batchLogger.write(s"$trial,$variationName run already exists - skipping\n")
                      } else {

                        Files.createDirectories(matsimConfig.io.experimentLoggingDirectory)

                        // todo: save the config to a file. posted to stacko:
                        //  https://stackoverflow.com/questions/60119481/standardized-method-for-writing-an-arbitrary-typesafe-config-to-a-hocon-file

                        val variationOutput: String           = variationHint.mkString("\n")
                        val variationOutputFile: File         = matsimConfig.io.experimentLoggingDirectory.resolve("variation.txt").toFile
                        val variationPrintWriter: PrintWriter = new PrintWriter(variationOutputFile.toString)
                        variationPrintWriter.write(variationOutput)
                        variationPrintWriter.close()

                        if (!variationPopFile.isFile) {
                          // generate the population
                          MATSimPopulationRunner.generateUniformPopulation(
                            matsimNetworkFile = matsimConfig.io.matsimNetworkFile,
                            popFileDestination = variationPopFile,
                            popSize = popSize,
                            adoptionRate = matsimConfig.routing.adoptionRate,
                            workActivityMinTime = matsimConfig.population.workActivityMinTime,
                            workActivityMaxTime = matsimConfig.population.workActivityMaxTime,
                            workDurationHours = matsimConfig.population.workDurationHours,
                            seed = Some {
                              seed
                            }
                          ) match {
                            case Left(e) =>
                              println(s"population generation for population-$trial exited with fatal error $e")
                              batchLogger.write(s"$trial,${e.toString}\n")
                            case Right(_) =>
                              ()
                          }
                        }

                        val experiment: MATSimExperimentRunner = MATSimExperimentRunner(matsimConfig, trial + batchSeed, Some { scenarioData })

                        Try {
                          experiment.run()
                        }.toEither match {
                          case Left(e) =>
                            println(s"trial $trial exit with fatal error $e")
                            batchLogger.write(s"$trial,${e.toString}\n")
                          case Right(result) =>
                            result match {
                              case Left(e) =>
                                println(s"trial $trial exit with handled error")
                                batchLogger.write(s"$trial,${e.toString}\n")
                              case Right(_) =>
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
