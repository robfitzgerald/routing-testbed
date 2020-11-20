package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.io.File
import java.nio.file.{Path, Paths}

import scala.util.Try

import cats.implicits._

import scalatikz.pgf.plots.Figure
import scalatikz.pgf.plots.enums.LegendPos
import scalatikz.pgf.enums.{Color, LineSize}
import scalatikz.pgf.enums.LineStyle.DASHED
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
              winnerMetrics <- AgentPerformanceMetrics.fromFiles(
                baselineFile,
                optimalFile,
                reportDisImprovedAgents = false
              )
              loserMetrics <- AgentPerformanceMetrics.fromFiles(baselineFile, optimalFile, reportImprovedAgents = false)
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
        .filter { file => file.isDirectory && file.getName != "selfish" }
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
    * @return for each trial, the AgentMetrics for that trial
    */
  def collectAgentMetrics(
    baseOutputDir: Path,
    experimentName: String,
    popSize: Int,
    trials: Int
  ): Either[Error, List[AgentMetrics]] = {

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
        selfishLoggingDir = s"$popSize-$trial-logging"
        selfishAgentExperiencePath = baseOutputDir
          .resolve("selfish")
          .resolve(selfishLoggingDir)
          .resolve("agentExperience.csv")
      } yield {
        (trial, selfishAgentExperiencePath.toFile)
      }
      val optimalFilesByTrialNumber = for {
        trial <- 0 until trials
        optimalAgentExperiencePath = baseOutputDir
          .resolve(experimentName)
          .resolve(trial.toString)
          .resolve("agentExperience.csv")
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
        } yield AgentMetrics(
          trial.toString,
          experimentName,
          overallMetrics,
          allPerformanceMetrics,
          winnerMetrics,
          loserMetrics
        )
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
      .map { colName => f"$prefix$colName" }
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
    * @return for each experiment name, a population-aggregated metric object
    */
  def createFromUngroupedMetricResults(completeMetrics: List[AgentMetrics]): List[PopulationAggregateMetrics] = {
    val result: Iterable[PopulationAggregateMetrics] = for {
      (experimentName, group) <- completeMetrics.groupBy { _.experimentName }
    } yield PopulationAggregateMetrics.from(experimentName, group)

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
  def toLaTeXPlots(rows: List[PGFPlotsData]): (String, String, String) = {
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
    val speed = for {
      row <- sortedRows
    } yield {
      f"(${row.experimentName}, ${row.speedNorm}%.2f) +- (0, ${row.speedStdev}%.2f)"
    }
    (tt.mkString("\n"), dist.mkString("\n"), speed.mkString("\n"))
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

  /**
    * strips all characters and attempts to read the string as a [[Double]]
    * @param numericExperimentName a possibly-numeric experiment name
    * @return the number, or, an error
    */
  def safeParse(numericExperimentName: String): Either[Error, Double] = {
    val result = for {
      replaced <- "([0-9.]+)".r
        .findFirstIn(numericExperimentName)
        .toRight(new Error(s"didn't find numbers in $numericExperimentName"))
      numeric <- Try { replaced.toDouble }.toEither.left.map { t =>
        new Error(s"couldn't parse $replaced as a Double", t)
      }
    } yield numeric
    result
  }

  def toTikzPlot(all: List[PGFPlotsData], losers: List[PGFPlotsData], parameterName: String = "Experiment") = {
    val allLookup   = all.map { r => (r.experimentName, r) }.toMap
    val loserLookup = losers.map { r => (r.experimentName, r) }.toMap
    val expNames    = allLookup.keySet.union(loserLookup.keySet).toList.sorted

    val result = for {
      domain <- expNames.traverse(safeParse)
    } yield {

      val (allTT, allTTσ)           = expNames.flatMap { allLookup.get(_).map { r => (r.ttNorm, r.ttStdev) } }.unzip
      val (allDist, allDistσ)       = expNames.flatMap { allLookup.get(_).map { r => (r.distNorm, r.distStdev) } }.unzip
      val (allSpeed, allSpeedσ)     = expNames.flatMap { allLookup.get(_).map { r => (r.speedNorm, r.speedStdev) } }.unzip
      val (loserTT, loserTTσ)       = expNames.flatMap { loserLookup.get(_).map { r => (r.ttNorm, r.ttStdev) } }.unzip
      val (loserDist, loserDistσ)   = expNames.flatMap { loserLookup.get(_).map { r => (r.distNorm, r.distStdev) } }.unzip
      val (loserSpeed, loserSpeedσ) = expNames.flatMap { loserLookup.get(_).map { r => (r.speedNorm, r.speedStdev) } }.unzip

      val save = Figure("Experiment")
        .plot(lineSize = LineSize.THICK, lineColor = Color.BLUE)(domain.zip(allTT))
        .plot(lineSize = LineSize.THICK, lineColor = Color.ORANGE)(domain.zip(allDist))
        .plot(lineSize = LineSize.THICK, lineColor = Color.MAGENTA)(domain.zip(allSpeed))
        .plot(lineSize = LineSize.THICK, lineColor = Color.BLUE, lineStyle = DASHED)(domain.zip(loserTT))
        .plot(lineSize = LineSize.THICK, lineColor = Color.ORANGE, lineStyle = DASHED)(domain.zip(loserDist))
        .plot(lineSize = LineSize.THICK, lineColor = Color.MAGENTA, lineStyle = DASHED)(domain.zip(loserSpeed))
        .havingLegends("TT", "DIST", "SPEED", "TT-NEG", "DIST-NEG", "SPEED-NEG")
        .havingLegendPos(LegendPos.OUTER_NORTH_EAST)
        .havingXLabel(s"$$$parameterName$$")
        .havingYLabel("$Percent$")
        .show()
//        .saveAsJPEG("test.jpg")

//      save match {
//        case util.Failure(err) =>
//          err
//        case util.Success(value) =>
//          value
//      }

      //        .errorBar() { domain.zip(allTT) } { (allTTσ.map{ stdev => 0.0 -> stdev }) }
      //        .plot(lineStyle = DASHED)(domain -> allDist)
      //        .havingTitle("$\\sin(x)$ vs $\\cos(x)$")
    }

    result
  }

  def meanStdev(xs: List[Double]): (Double, Double) = {
    if (xs.isEmpty) (0.0, 0.0)
    else {
      val mean: Double     = xs.sum / xs.length
      val variance: Double = xs.map { n => math.pow(n - mean, 2) }.sum / xs.length
      val stdev: Double    = math.sqrt(variance)
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
  def performanceMetricsPGFPlotsAgg(
    xs: List[AgentPerformanceMetrics]
  ): (Double, Double, Double, Double, Double, Double) = {
    val (ttDiffNormAll, ttDiffNormStdevAll)       = asPercents(meanStdev(xs.map { _.ttDiffNorm }))
    val (distDiffNormAll, distDiffNormStdevAll)   = asPercents(meanStdev(xs.map { _.distDiffNorm }))
    val (speedDiffNormAll, speedDiffNormStdevAll) = asPercents(meanStdev(xs.map { _.speedDiffNorm }))
    (ttDiffNormAll, ttDiffNormStdevAll, distDiffNormAll, distDiffNormStdevAll, speedDiffNormAll, speedDiffNormStdevAll)
  }
}
