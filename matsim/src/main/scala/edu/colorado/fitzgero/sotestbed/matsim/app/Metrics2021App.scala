package edu.colorado.fitzgero.sotestbed.matsim.app

import java.nio.file.Path

import cats.implicits._

import com.monovore.decline._
import edu.colorado.fitzgero.sotestbed.matsim.analysis.{
  AgentExperienceOps,
  BatchDataOps,
  BatchMetrics,
  PopulationAggregateMetrics
}

/**
  * reads agent experience and batch data files to produce aggregate metrics of an experiment
  */
object Metrics2021App
    extends CommandApp(
      name = "Metrics 2021 App",
      header = "a single row combining agent and batch-level stats",
      main = {

        val dirOpt: Opts[Path] =
          Opts.option[Path](long = "directory", short = "d", help = "directory containing experiment sub-directories")

        val expFileOpt: Opts[Option[Path]] =
          Opts
            .option[Path](long = "experiment_file", short = "e", help = "file with list of experiments")
            .orNone

        val algorithmNamesOpt: Opts[String] =
          Opts.option[String](long = "algorithm_names", short = "n", help = "comma-delimited experiment names to query")

        (dirOpt, expFileOpt, algorithmNamesOpt).mapN {
          case (dir, expFile, algNames) =>
            // load all files in the directory

            val expDirs: Iterator[Path] = expFile match {
              case Some(value) =>
                val source = scala.io.Source.fromFile(value.toFile)
                val exps = for {
                  expDirName <- source.getLines
                } yield {
                  dir.resolve(expDirName)
                }
                source.close()
                exps
              case None =>
                // fails here, not a valid directory (todo: make safe)
                dir.toFile.listFiles(_.isDirectory).map { _.toPath }.toIterator
            }

            val result: List[Option[(String, (List[PopulationAggregateMetrics], List[String], List[BatchMetrics]))]] =
              for {
                expDir <- expDirs.toList
                dataDirectory = dir.resolve(expDir)
                alg <- algNames.split(",")
              } yield {
                val expTag = s"${expDir.getFileName}-$alg"
                val analysisResult = for {
                  completeMetrics <- AgentExperienceOps.collectAgentMetrics(dataDirectory, alg, -1, 1)
                  batchMetrics    <- BatchDataOps.collectBatchMetrics(dataDirectory, alg, 1)
                } yield {
                  val rows    = AgentExperienceOps.createFromUngroupedMetricResults(completeMetrics)
                  val csvRows = rows.map { _.outputString }
                  (rows, csvRows, batchMetrics)
                }
                analysisResult match {
                  case Left(e) =>
                    println(s"skipping trial $expDir")
                    println(s"reason: ${e.getMessage} ${e.getCause}")
                    println(e)
                    None
                  case Right(printableOutputs) =>
                    Some((expTag, printableOutputs))
                }
              }

            println("\nMETRICS SUBSET REPORT\n")
            val header =
              s"experimentName,ttAll,ttσAll,speedAll,speedσAll,distAll,distσAll,ttLose,ttσLose,speedLose,speedσLose,distLose,distσLose," +
                s"${BatchMetrics.Header}," +
                s"trials"
            println(header)
            for {
              (expName, (rawRows, _, batchMetricsList)) <- result.flatten
            } {
              val batchAgg = batchMetricsList.foldLeft(BatchMetrics()) { _.add(_) }
              val stats = for {
                popAggMetrics <- rawRows
              } yield {
//                val experimentName = popAggMetrics.allData.experimentName
                val ttAll       = popAggMetrics.allData.ttNorm
                val ttσAll      = popAggMetrics.allData.ttStdev
                val speedAll    = popAggMetrics.allData.speedNorm
                val speedσAll   = popAggMetrics.allData.speedStdev
                val distAll     = popAggMetrics.allData.distNorm
                val distσAll    = popAggMetrics.allData.distStdev
                val ttLoser     = popAggMetrics.loserData.ttNorm
                val ttσLoser    = popAggMetrics.loserData.ttStdev
                val speedLoser  = popAggMetrics.loserData.speedNorm
                val speedσLoser = popAggMetrics.loserData.speedStdev
                val distLoser   = popAggMetrics.loserData.distNorm
                val distσLoser  = popAggMetrics.loserData.distStdev
                val row =
                  f"$expName,$ttAll%.2f%%,$ttσAll%.2f%%,$speedAll%.2f%%,$speedσAll%.2f%%,$distAll%.2f%%,$distσAll%.2f%%,$ttLoser%.2f%%,$ttσLoser%.2f%%,$speedLoser%.2f%%,$speedσLoser%.2f%%,$distLoser%.2f%%,$distσLoser%.2f%%,${batchAgg.toString},${popAggMetrics.count}"
                println(row)

              }
            }
//            val rawRows = result.flatMap { _._1 }.toList
//            AgentExperienceOps.toTikzPlot(rawRows.map { _.allData }, rawRows.map { _.loserData })
        }
      }
    )
