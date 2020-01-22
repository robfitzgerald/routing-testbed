package edu.colorado.fitzgero.sotestbed.algorithm.batching

object BatchSplittingFunction {
  /**
    * uses a sliding window to split groups which are too large
    * @param batch batch to split
    * @param maxBatchSize user-imposed maximum batch size
    * @return the batch, broken out to smaller batches if needed
    */
  def bySlidingWindow[T](batch: List[T], maxBatchSize: Int): List[List[T]] =
    batch.sliding(maxBatchSize, maxBatchSize).toList

  /**
    * continually splits batches in half until all resulting sub batches are the correct size.
    * more likely to end up with ideal batch sizes than the sliding window method
    * @param batch batch to split
    * @param maxBatchSize user-imposed maximum batch size
    * @return the batch, broken out to smaller batches if needed
    */
  def splitUntilValid[T](batch: List[T], maxBatchSize: Int): List[List[T]] = {
    @scala.annotation.tailrec
    def _split(solution: List[List[T]]): List[List[T]] = {
      if (solution.forall(_.lengthCompare(maxBatchSize) <= 0)) {
        solution
      } else {
        val nextSolution: List[List[T]] = solution.flatMap { subBatch =>
          if (subBatch.lengthCompare(maxBatchSize) > 0) {
            val (a, b) = subBatch.splitAt(subBatch.length / 2)
            List(a, b)
          } else {
            List(subBatch)
          }
        }
        _split(nextSolution)
      }
    }
    _split(List(batch))
  }

  def takeOnlyOne[T](batch: List[T], maxBatchSize: Int): List[List[T]] =
    List(batch.take(maxBatchSize))
}
