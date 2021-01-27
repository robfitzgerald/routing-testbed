package edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.batchoverlap

import edu.colorado.fitzgero.sotestbed.model.agent.Request

/**
  * the computed overlap values for each agent
  */
case class BatchOverlapResult(overlapValues: Map[Request, Double])
