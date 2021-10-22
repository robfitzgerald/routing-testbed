package edu.colorado.fitzgero.sotestbed.matsim.config.generator

sealed trait BatchingFunction

object BatchingFunction {

  case object NoBatching extends BatchingFunction

  case class TrajectoryClustering(
    omegaDelta: Double = 0.5,
    omegaBeta: Double = 0.5,
    omegaA: Double = 0.5,
    omegaS: Double = 0.5,
    runtimeMs: Int = 10000,
    trajectoryHistoryTimeLimitSeconds: Int = 300
  ) extends BatchingFunction

  implicit class BatchingFunctionInstance(batchingFunction: BatchingFunction) {

    def toHocon: String = batchingFunction match {
      case TrajectoryClustering(omegaDelta, omegaBeta, omegaA, omegaS, runtimeMs, trajectoryHistoryTimeLimitSeconds) =>
        s"""algorithm.batching-function = {
           |  type = label-based-trajectory-clustering
           |  omega-delta = $omegaDelta
           |  omega-beta = $omegaBeta
           |  omega-a = $omegaA
           |  omega-s = $omegaS
           |  max-iterations = 100
           |  max-runtime-milliseconds = $runtimeMs
           |  trajectory-time-limit = $trajectoryHistoryTimeLimitSeconds
           |}""".stripMargin
      case NoBatching =>
        s"""algorithm.batching-function.type = no-batching"""
    }
  }
}
