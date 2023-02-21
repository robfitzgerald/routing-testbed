package edu.colorado.fitzgero.sotestbed.algorithm.selection

import edu.colorado.fitzgero.sotestbed.model.numeric.RunTime

final case class SelectionRunnerResult(
  batchId: String,
  selection: SelectionAlgorithm.SelectionAlgorithmResult,
  runtime: RunTime
)
