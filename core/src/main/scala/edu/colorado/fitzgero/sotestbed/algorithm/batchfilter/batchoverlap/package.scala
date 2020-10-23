package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path}

package object batchoverlap {

  type BatchOverlapFunction = Map[Request, List[Path]] => BatchOverlapResult

  type PathOverlapLookup = List[Path] => List[(EdgeId, Cost)]
}
