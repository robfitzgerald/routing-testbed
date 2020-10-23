package edu.colorado.fitzgero.sotestbed.matsim.analysis

import java.nio.file.Path

import edu.colorado.fitzgero.sotestbed.util.CombinedError
import kantan.csv.ReadError

object BatchDataOps {

  def collectBatchMetrics(baseDir: Path, experimentName: String, trials: Int): Either[Error, List[BatchMetrics]] = {
    if (!baseDir.toFile.isDirectory) {
      Left(new Error(s"$baseDir is not a directory"))
    } else {
      case class Acc(
        batchMetrics: List[BatchMetrics] = List.empty,
        errors: List[ReadError] = List.empty
      )
      val trialNumbers: List[Int] = (0 until trials).toList
      val result = trialNumbers.foldLeft(Acc()) { (acc, trial) =>
        val batchDataFilePath = baseDir.resolve(experimentName).resolve(trial.toString).resolve("batchLearning.csv")
        BatchMetrics.fromFile(batchDataFilePath.toFile) match {
          case Left(error) =>
            acc.copy(errors = error +: acc.errors)
          case Right(value) =>
            acc.copy(batchMetrics = value +: acc.batchMetrics)
        }
      }
      if (result.errors.nonEmpty) {
        Left(CombinedError(result.errors))
      } else {
        Right(result.batchMetrics)
      }
    }
  }
}
