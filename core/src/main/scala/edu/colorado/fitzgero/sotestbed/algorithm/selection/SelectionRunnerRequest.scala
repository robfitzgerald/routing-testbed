package edu.colorado.fitzgero.sotestbed.algorithm.selection

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork._

final case class SelectionRunnerRequest(
  batchId: String,
  finalAlternatePaths: Map[Request, List[Path]]
)

object SelectionRunnerRequest {

  def fromTspResult(
    batchId: String,
    finalPaths: List[(Request, Path)]
  ): SelectionRunnerRequest = SelectionRunnerRequest(
    batchId,
    finalPaths.map { case (req, path) => (req, List(path)) }.toMap
  )
}
