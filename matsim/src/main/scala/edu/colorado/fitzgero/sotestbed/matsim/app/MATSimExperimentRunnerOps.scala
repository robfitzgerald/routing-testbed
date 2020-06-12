package edu.colorado.fitzgero.sotestbed.matsim.app

import java.io.{File, PrintWriter}

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimRunConfig
import edu.colorado.fitzgero.sotestbed.util.SummaryStats
import kantan.csv._
import kantan.csv.ops._

object MATSimExperimentRunnerOps extends LazyLogging {

  val FairnessHeader: String = "gUFMean,gUFStdev,gUFSkew,gUFKurt,gCntImpr,gPctImpr,soUFMean,soUFStdev,soUFSkew,soUFKurt,soCntImpr,soPctImpr,numTrips"

  /**
    * computes the fairness statistics for a SO run, producing a set of fairness histograms as a side-effect
    * @param config the matsim run configuration, used for file locations
    * @return a collection of statistics
    */
  def fairness(config: MATSimRunConfig): Either[Exception, String] = {
    Try {
      val selfishConfig =
        config.copy(
          scenarioData = config.scenarioData.copy(
            algorithm = "selfish",
          )
        )
      val selfishAgentExperienceFile =
        selfishConfig.experimentLoggingDirectory
          .resolve("agentExperience.csv")
          .toFile
      val optimalAgentExperienceFile =
        config.experimentLoggingDirectory
          .resolve("agentExperience.csv")
          .toFile

      case class AgentExperienceRow(
        agentId: String,
        requestClass: String,
        departureTime: Int,
        travelTime: Double,
        distance: Double,
        replannings: Int
      )
      implicit val dec: HeaderDecoder[AgentExperienceRow] =
        HeaderDecoder.decoder(
          "agentId",
          "requestClass",
          "departureTime",
          "travelTime",
          "distance",
          "replannings"
        ) { AgentExperienceRow.apply }

      // pull in all data
      val selfishRows: Map[String, AgentExperienceRow] =
        selfishAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .map { row =>
            s"${row.agentId}#${row.departureTime}" -> row
          }
          .toMap

      // global rows considers the number of agents
      val (optimalRowsGlobal: Map[String, AgentExperienceRow], optimalRowsOnlySO: Map[String, AgentExperienceRow]) =
        optimalAgentExperienceFile
          .unsafeReadCsv[List, AgentExperienceRow](rfc.withHeader)
          .foldLeft((Map.empty[String, AgentExperienceRow], Map.empty[String, AgentExperienceRow])) { (acc, row) =>
            val (global, soOnly)                               = acc
            val tag                                            = s"${row.agentId}#${row.departureTime}"
            val updatedGlobal: Map[String, AgentExperienceRow] = global.updated(tag, row)
            val updatedSO: Map[String, AgentExperienceRow]     = if (row.requestClass == "ue") soOnly else soOnly.updated(tag, row)
            (updatedGlobal, updatedSO)
          }

      val numTrips: Int = selfishRows.size

      case class Accumulator(
        values: List[Double] = List.empty,
        stats: SummaryStats = SummaryStats(),
        countImproved: Int = 0
      ) {
        def addObservation(obs: (String, AgentExperienceRow)): Accumulator = {
          val (agentTimeIndex, optimalRow) = obs
          selfishRows.get(agentTimeIndex) match {
            case None =>
              this
            case Some(selfishRow) =>
              val travelTimeDiff: Double = selfishRow.travelTime - optimalRow.travelTime
              val updatedStats           = this.stats.add(travelTimeDiff)
              val updatedCount: Int      = if (travelTimeDiff < 0.0) this.countImproved + 1 else this.countImproved
              this.copy(
                values = travelTimeDiff +: this.values,
                stats = updatedStats,
                countImproved = updatedCount
              )
          }
        }

        override def toString: String = {
          val percentImproved: Double = (this.countImproved.toDouble / numTrips.toDouble) * 100.0
          f"${this.stats},$countImproved,$percentImproved%.2f%%"
        }
      }
      val globalStats: Accumulator = optimalRowsGlobal.foldLeft(Accumulator()) { _.addObservation(_) }
      val soOnlyStats: Accumulator = optimalRowsOnlySO.foldLeft(Accumulator()) { _.addObservation(_) }

      val globalHistogramFile = config.experimentLoggingDirectory.resolve("global.hist").toFile
      histogramMe(globalStats.values, globalHistogramFile)
      val soOnlyHistogramFile = config.experimentLoggingDirectory.resolve("so.hist").toFile
      histogramMe(soOnlyStats.values, soOnlyHistogramFile)

      s"$globalStats,$soOnlyStats,$numTrips"

    }.toEither.left.map { t =>
      new Exception(t)
    }
  }

  /**
    * use code stolen from our previous SO routing testbed to compute histograms
    * @param xs input data
    * @param file output destination, LaTeX PGFPlot-formatted histogram data
    */
  def histogramMe(xs: Iterable[Double], file: File, bins: Int = 20): Unit = {
    // https://stackoverflow.com/questions/24536215/scala-simple-histogram
    def histogram(n_bins: Int, lowerUpperBound: Option[(Double, Double)] = None)(xs: Iterable[Double]): Iterator[((Double, Double), Int)] = {
      val (mn, mx)                                      = lowerUpperBound.getOrElse(xs.min, xs.max)
      val epsilon                                       = 0.0001
      val binSize: Double                               = (mx - mn) / n_bins * (1 + epsilon)
      val bins: Iterator[(Double, Double)]              = (0 to n_bins).map(mn + _ * binSize).sliding(2).map(xs => (xs(0), xs(1)))
      def binContains(bin: (Double, Double), x: Double) = (x >= bin._1) && (x < bin._2)
      bins.map(bin => (bin, xs.count(binContains(bin, _))))
    }

    /**
      * prints out the histogram values in LaTex PGFPlots data format
      * @param hist
      * @param maxValue
      * @return
      */
    def printHistogram(hist: Iterator[((Double, Double), Int)], maxValue: Double): String =
      hist.map { case ((min, _), count) => s"($min, $count)" }.mkString(" ").concat(s" ($maxValue, 0)")

    if (xs.nonEmpty) {
      val histogramData: Iterator[((Double, Double), Int)] = histogram(n_bins = bins, lowerUpperBound = None)(xs)
      val maxValue: Double                                 = xs.max
      val output: String                                   = printHistogram(histogramData, maxValue)
      Try {
        val pw: PrintWriter = new PrintWriter(file)
        pw.write(output)
        pw.close()
      }.toEither match {
        case Left(error) =>
          logger.error(s"could not write histogram to file $file due to:")
          logger.error(s"${error.getClass} ${error.getMessage} ${error.getCause}")
        case Right(_) =>
          logger.info(s"histogram written to $file")
      }
    }
  }
}
