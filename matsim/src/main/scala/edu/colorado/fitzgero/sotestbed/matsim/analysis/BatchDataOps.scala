package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.nio.file.Path

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.util.CombinedError
import kantan.csv.ReadError

object BatchDataOps extends LazyLogging {

  def collectBatchMetrics(
    baseDir: Path,
    experimentName: String,
    trials: Int,
    ignoreErrors: Boolean = true
  ): Either[Error, List[BatchMetrics]] = {
    if (!baseDir.toFile.isDirectory) {
      Left(new Error(s"$baseDir is not a directory"))
    } else {
      case class Acc(
        batchMetrics: List[BatchMetrics] = List.empty,
        errors: List[Error] = List.empty
      )
      val trialNumbers: List[Int] = (0 until trials).toList
      val result = trialNumbers.foldLeft(Acc()) { (acc, trial) =>
        val batchDataFilePath = baseDir.resolve(experimentName).resolve(trial.toString).resolve("batchData.csv")
        BatchMetrics.fromFile(batchDataFilePath.toFile) match {
          case Left(error) =>
            val msg            = s"file $batchDataFilePath"
            val explainedError = new Error(msg, error)
            logger.warn(s"failure reading batch data for $experimentName trial $trial", explainedError)
            acc.copy(errors = explainedError +: acc.errors)
          case Right(value) =>
            acc.copy(batchMetrics = value +: acc.batchMetrics)
        }
      }
      if (!ignoreErrors && result.errors.nonEmpty) {
        Left(CombinedError(result.errors, Some(s"failure collecting batch metrics")))
      } else {
        Right(result.batchMetrics)
      }
    }
  }
}
