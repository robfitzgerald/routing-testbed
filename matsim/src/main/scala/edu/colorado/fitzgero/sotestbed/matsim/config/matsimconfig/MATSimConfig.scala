package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.io.File
import java.nio.file.{Path, Paths}

import scala.concurrent.duration.Duration

import edu.colorado.fitzgero.sotestbed.config.algorithm.{BatchingFunctionConfig, CombineFlowsFunctionConfig, KSPAlgorithmConfig, MarginalCostFunctionConfig, PathToMarginalFlowsFunctionConfig, SelectionAlgorithmConfig}
import edu.colorado.fitzgero.sotestbed.model.numeric.{SimTime, TravelTimeSeconds}

final case class MATSimConfig(
  io: MATSimConfig.IO,
  run: MATSimConfig.Run,
  routing: MATSimConfig.Routing,
  algorithm: MATSimConfig.Algorithm
)

object MATSimConfig {

  final case class Run(
    lastIteration: Int,
    soRoutingIterationCycle: Int,
    soFirstIteration: Boolean,
    startOfSimTime: SimTime,
    endOfSimTime: SimTime,
    endOfRoutingTime: SimTime,
    matsimStepSize: SimTime,
    matsimSemaphoreTimeoutMs: Long,
    simulationTailTimeout: Duration
  ) {
    require(soRoutingIterationCycle <= lastIteration,
            "matsimConfig.run.soRoutingIterationCycle needs to be less than or equal to matsimConfig.run.lastIteration")
    require(matsimSemaphoreTimeoutMs > 0, "matsimConfig.run.matsimSemaphoreTimeoutMs must be positive")
  }

  final case class Routing(
    batchWindow: SimTime,
    maxPathAssignments: Int,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    minimumReplanningWaitTime: SimTime,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds,
    requestUpdateCycle: SimTime,
  ) {
    require(requestUpdateCycle > SimTime.Zero, "matsimConfig.routing.requestUpdateCycle needs to be at least 1")
  }

  final case class IO(
    matsimNetworkFile: File,
    populationFile: File,
    matsimConfigFile: File,
    experimentTimestamp: Long = System.currentTimeMillis,
    workingBaseDirectory: Path = Paths.get("/tmp"),
    matsimLogLevel: String = "INFO",
    name: String = "so-matsim"
  ) {

    def workingDirectory: Path = this.workingBaseDirectory.resolve(s"$experimentTimestamp")

    def experimentSubdirectoryName: String =
      s"$name-${System.currentTimeMillis}"
  }

  final case class Algorithm(
    kspAlgorithm: KSPAlgorithmConfig,
    selectionAlgorithm: SelectionAlgorithmConfig,
    pathToMarginalFlowsFunction: PathToMarginalFlowsFunctionConfig,
    combineFlowsFunction: CombineFlowsFunctionConfig,
    marginalCostFunction: MarginalCostFunctionConfig,
    batchingFunction: BatchingFunctionConfig
  )
}
