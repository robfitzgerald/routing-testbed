package edu.colorado.fitzgero.sotestbed.algorithm.batching

object BatchSplittingFunction {
  /**
    * uses a sliding window to split groups which are too large
    * @param batch batch to split
    * @param maxBatchSize user-imposed maximum batch size
    * @return the batch, broken out to smaller batches if needed
    */
  def bySlidingWindow(batch: List[AgentBatchData], maxBatchSize: Int): List[List[AgentBatchData]] =
    batch.sliding(maxBatchSize, maxBatchSize).toList

  /**
    * continually splits batches in half until all resulting sub batches are the correct size.
    * more likely to end up with ideal batch sizes than the sliding window method
    * @param batch batch to split
    * @param maxBatchSize user-imposed maximum batch size
    * @return the batch, broken out to smaller batches if needed
    */
  def splitUntilValid(batch: List[AgentBatchData], maxBatchSize: Int): List[List[AgentBatchData]] = {
    @scala.annotation.tailrec
    def _split(solution: List[List[AgentBatchData]]): List[List[AgentBatchData]] = {
      if (solution.forall(_.lengthCompare(maxBatchSize) <= 0)) {
        solution
      } else {
        val nextSolution: List[List[AgentBatchData]] = solution.flatMap { subBatch =>
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

  def takeOnlyOne(batch: List[AgentBatchData], maxBatchSize: Int): List[List[AgentBatchData]] =
    List(batch.take(maxBatchSize))
}
