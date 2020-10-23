package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File
import java.nio.file.{Path, Paths}

import edu.colorado.fitzgero.sotestbed.util.CombinedError

object AgentExperienceOps {

  /**
    * old way of collecting output, replaced by AgentExperienceOps.collectMetrics2
    *
    * @param dir
    * @param refFileName
    * @return
    * @deprecated
    */
  def collectMetrics(dir: Path, refFileName: String): Either[Error, List[AgentMetrics]] = {
    if (!dir.toFile.isDirectory) {
      Left(new Error(s"$dir is not a directory"))
    } else {

      val outerResult: List[Either[Exception, AgentMetrics]] = {
        for {
          subDir <- dir.toFile.listFiles.toList.filter { _.isDirectory }.sorted
          nodeName = subDir.getName // cluster node name
        } yield {

          val baselineFile = Paths.get(subDir.toString).resolve(refFileName).toFile

          val optimalFiles = subDir.listFiles.toList.filter { file =>
            file.getName != refFileName && !file.getName.startsWith(".")
          }.sorted

          val innerResult: List[Either[Exception, AgentMetrics]] = for {
            optimalFile <- optimalFiles
            fileName = optimalFile.getName.substring(0, optimalFile.getName.length - 4) // hack to remove ".csv"
          } yield {

            println(s"collecting stats for node $nodeName file ${optimalFile.getName}")

            // compare this optimal result against the baseline
            for {
              overallMetrics        <- AgentBaseMetrics(optimalFile)
              allPerformanceMetrics <- AgentPerformanceMetrics.fromFiles(baselineFile, optimalFile)
              winnerMetrics         <- AgentPerformanceMetrics.fromFiles(baselineFile, optimalFile, reportDisImprovedAgents = false)
              loserMetrics          <- AgentPerformanceMetrics.fromFiles(baselineFile, optimalFile, reportImprovedAgents = false)
            } yield AgentMetrics(nodeName, fileName, overallMetrics, allPerformanceMetrics, winnerMetrics, loserMetrics)
          }

          innerResult
        }
      }.flatten

      val errors: List[Exception] = outerResult.flatMap {
        case Right(_)    => None
        case Left(error) => Some(error)
      }

      if (errors.nonEmpty) {
        val errorResult = CombinedError(errors)
        Left(errorResult)
      } else {
        val results: List[AgentMetrics] = outerResult.flatMap {
          case Left(_)      => None
          case Right(stats) => Some(stats)
        }
        Right(results)
      }
    }
  }

  def allSODirectoriesUnder(baseOutputDir: Path): List[String] = {
    val optimalFiles: List[String] =
      baseOutputDir.toFile.listFiles.toList
        .filter { file =>
          file.isDirectory && file.getName != "selfish"
        }
        .sorted
        .map { _.getName }

    optimalFiles
  }

  /**
    * given the output directory structure of a SO run, and the name of an experiment,
    * compute the metrics comparing the results against their selfish baselines
    *
    * @param baseOutputDir the base directory of the experiment output
    * @param experimentName the directory name under the base directory we are collecting SO results from
    * @param popSize the population size for the experiments we are analyzing
    * @param trials the number of trials we are collecting
    * @return
    */
  def collectMetrics2(baseOutputDir: Path, experimentName: String, popSize: Int, trials: Int): Either[Error, List[AgentMetrics]] = {

    if (!baseOutputDir.toFile.isDirectory) {
      Left(new Error(s"$baseOutputDir is not a directory"))
    } else {

      // expected layout:
      //   dir/
      //     $studyName/
      //       $trial
      //         agentExperience.csv   - optimal result
      //     selfish/
      //       $popSize-$trial-logging/
      //         agentExperience.csv   - selfish result

      // construct the selfish and optimal file paths by their trial number
      val selfishFilesByTrialNumber = for {
        trial <- 0 until trials
        selfishLoggingDir          = s"$popSize-$trial-logging"
        selfishAgentExperiencePath = baseOutputDir.resolve("selfish").resolve(selfishLoggingDir).resolve("agentExperience.csv")
      } yield {
        (trial, selfishAgentExperiencePath.toFile)
      }
      val optimalFilesByTrialNumber = for {
        trial <- 0 until trials
        optimalAgentExperiencePath = baseOutputDir.resolve(experimentName).resolve(trial.toString).resolve("agentExperience.csv")
      } yield {
        (trial, optimalAgentExperiencePath.toFile)
      }

      val selfishFileLookup: Map[Int, File] = selfishFilesByTrialNumber.toMap
      val optimalFileLookup: Map[Int, File] = optimalFilesByTrialNumber.toMap

      val result = for {
        trial       <- (0 until trials).toList
        selfishFile <- selfishFileLookup.get(trial)
        optimalFile <- optimalFileLookup.get(trial)
      } yield {
        for {
          overallMetrics        <- AgentBaseMetrics(optimalFile)
          allPerformanceMetrics <- AgentPerformanceMetrics.fromFiles(selfishFile, optimalFile)
          winnerMetrics         <- AgentPerformanceMetrics.fromFiles(selfishFile, optimalFile, reportDisImprovedAgents = false)
          loserMetrics          <- AgentPerformanceMetrics.fromFiles(selfishFile, optimalFile, reportImprovedAgents = false)
        } yield AgentMetrics(trial.toString, experimentName, overallMetrics, allPerformanceMetrics, winnerMetrics, loserMetrics)
      }

      val errors: List[Exception] = result.flatMap {
        case Right(_)    => None
        case Left(error) => Some(error)
      }

      if (errors.nonEmpty) {
        val errorResult = CombinedError(errors)
        Left(errorResult)
      } else {
        val results: List[AgentMetrics] = result.flatMap {
          case Left(_)      => None
          case Right(stats) => Some(stats)
        }
        Right(results)
      }
    }
  }

  def perfHeader(prefix: String): String = {
    val defaultNames: String = f"ttDiff,ttDiffStdev,ttDiffNorm,ttDiffNormStdev," +
      f"distDiff,distDiffStdev,distDiffNorm,distDiffNormStdev," +
      f"speedDiff,speedDiffStdev,speedDiffNorm,speedDiffNormStdev"
    defaultNames
      .split(",")
      .map { colName =>
        f"$prefix$colName"
      }
      .mkString(",")
  }

  val AggregateOutputHeader: String =
    "experimentName,trials,ttMin,ttStdev,distMiles,distStdev,speedMph,speedStdev,winPercent,losePercent," +
      s"${perfHeader("all_")},${perfHeader("win_")},${perfHeader("lose_")}"

  /**
    * given a list of experiment trial results, group them and aggregate the mean/stdev values into
    * the final row form of an output CSV
    *
    * @param completeMetrics
    * @return
    */
  def createFromUngroupedMetricResults(completeMetrics: List[AgentMetrics]): List[(String, PGFPlotsData, Double, Double, PGFPlotsData)] = {
    val result: Iterable[(String, PGFPlotsData, Double, Double, PGFPlotsData)] = for {
      (experimentName, group) <- completeMetrics.groupBy { _.experimentName }
    } yield {

      val trials: Int = group.length

      // overall metrics
      val (ttMin, ttStdev)       = meanStdev(group.map { _.overallMetrics.travelTimeMinutes })
      val (distMiles, distStdev) = meanStdev(group.map { _.overallMetrics.distanceMiles })
      val (speedMph, speedStdev) = meanStdev(group.map { _.overallMetrics.speedMph })
      val overallMetrics         = f"$ttMin%.4f,$ttStdev%.4f,$distMiles%.4f,$distStdev%.4f,$speedMph%.4f,$speedStdev%.4f"

      // all performance metrics
      val allPerf: String  = performanceMetricsAgg(group.map { _.allPerformanceMetrics })
      val winPerf: String  = performanceMetricsAgg(group.map { _.winnerMetrics })
      val losePerf: String = performanceMetricsAgg(group.map { _.loserMetrics })

      // percent of agents winning/losing
      val overallCount: Int                                          = group.map { _.allPerformanceMetrics.count }.foldLeft(0) { _ + _ }
      val winCount: Int                                              = group.map { _.winnerMetrics.count }.foldLeft(0) { _ + _ }
      val loseCount: Int                                             = group.map { _.loserMetrics.count }.foldLeft(0) { _ + _ }
      val winPercent: Double                                         = (winCount.toDouble / overallCount.toDouble) * 100.0
      val losePercent: Double                                        = (loseCount.toDouble / overallCount.toDouble) * 100.0
      val (ttNormAll, ttNormStdevAll, distNormAll, distNormStdevAll) = performanceMetricsPGFPlotsAgg(group.map { _.allPerformanceMetrics })
      val (ttNormL, ttNormStdevL, distNormL, distNormStdevL)         = performanceMetricsPGFPlotsAgg(group.map { _.loserMetrics })

      val outputString = f"$experimentName,$trials,$overallMetrics,$winPercent%%,$losePercent%%,$allPerf,$winPerf,$losePerf"
      val allData = PGFPlotsData(
        experimentName,
        winPercent,
        losePercent,
        ttNormAll,
        ttNormStdevAll,
        distNormAll,
        distNormStdevAll
      )

      val loserData = PGFPlotsData(
        experimentName,
        winPercent,
        losePercent,
        ttNormL,
        ttNormStdevL,
        distNormL,
        distNormStdevL
      )

      (outputString, allData, winPercent, losePercent, loserData)
    }

    result.toList
  }

  /**
    * convert related data across columns into a pgfplot coordinate list
    *
    * expects that experimentName can be cast as numeric and sorted, and if not,
    * will sort based on lexicographical sort on experimentName as a String
    *
    * @param rows
    * @return Travel Time coordinates, Distance coordinates
    */
  def toLaTeXPlots(rows: List[PGFPlotsData]): (String, String) = {
    val sortedRows = rows.sorted(PGFPlotsData.pickOrdering(rows))
    val tt = for {
      row <- sortedRows
    } yield {
      f"(${row.experimentName}, ${row.ttNorm}%.2f) +- (0, ${row.ttStdev}%.2f)"
    }
    val dist = for {
      row <- sortedRows
    } yield {
      f"(${row.experimentName}, ${row.distNorm}%.2f) +- (0, ${row.distStdev}%.2f)"
    }
    (tt.mkString("\n"), dist.mkString("\n"))
  }

  def toLaTeXWinLosePlot(rows: List[PGFPlotsData]): (String, String) = {
    val sortedRows = rows.sorted(PGFPlotsData.pickOrdering(rows))
    val win = for {
      row <- sortedRows
    } yield {
      f"(${row.experimentName}, ${row.winPct}%.2f)"
    }
    val lose = for {
      row <- sortedRows
    } yield {
      f"(${row.experimentName}, ${row.losePct}%.2f)"
    }
    (win.mkString("\n"), lose.mkString("\n"))
  }

  def meanStdev(xs: List[Double]): (Double, Double) = {
    if (xs.isEmpty) (0.0, 0.0)
    else {
      val mean: Double = xs.sum / xs.length
      val variance: Double = xs.map { n =>
        math.pow(n - mean, 2)
      }.sum / xs.length
      val stdev: Double = math.sqrt(variance)
      (mean, stdev)
    }
  }

  def performanceMetricsAgg(xs: List[AgentPerformanceMetrics]): String = {
    val (ttDiffAll, ttDiffStdevAll)               = meanStdev(xs.map { _.ttDiffMin })
    val (ttDiffNormAll, ttDiffNormStdevAll)       = asPercents(meanStdev(xs.map { _.ttDiffNorm }))
    val (distDiffAll, distDiffStdevAll)           = meanStdev(xs.map { _.distDiffMiles })
    val (distDiffNormAll, distDiffNormStdevAll)   = asPercents(meanStdev(xs.map { _.distDiffNorm }))
    val (speedDiffAll, speedDiffStdevAll)         = meanStdev(xs.map { _.speedDiffMph })
    val (speedDiffNormAll, speedDiffNormStdevAll) = asPercents(meanStdev(xs.map { _.speedDiffNorm }))
    f"$ttDiffAll%.4f%%,$ttDiffStdevAll%.4f%%,$ttDiffNormAll%.4f%%,$ttDiffNormStdevAll%.4f%%," +
      f"$distDiffAll%.4f%%,$distDiffStdevAll%.4f%%,$distDiffNormAll%.4f%%,$distDiffNormStdevAll%.4f%%," +
      f"$speedDiffAll%.4f%%,$speedDiffStdevAll%.4f%%,$speedDiffNormAll%.4f%%,$speedDiffNormStdevAll%.4f%%"
  }

  def asPercents(meanStdev: (Double, Double)): (Double, Double) = {
    val (mean, stdev) = meanStdev
    (mean * 100.0, stdev * 100.0)
  }

  /**
    * get the mean tt, stdev tt, mean dist, and stdev dist for a set of performance metrics
    * @param xs performance metrics for a common experiment
    * @return specific normalized mean/stdev values
    */
  def performanceMetricsPGFPlotsAgg(xs: List[AgentPerformanceMetrics]): (Double, Double, Double, Double) = {
    val (ttDiffNormAll, ttDiffNormStdevAll)     = asPercents(meanStdev(xs.map { _.ttDiffNorm }))
    val (distDiffNormAll, distDiffNormStdevAll) = asPercents(meanStdev(xs.map { _.distDiffNorm }))
    (ttDiffNormAll, ttDiffNormStdevAll, distDiffNormAll, distDiffNormStdevAll)
  }
}
