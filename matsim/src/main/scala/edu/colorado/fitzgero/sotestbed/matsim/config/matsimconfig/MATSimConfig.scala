package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.io.File
import java.nio.file.{Path, Paths}

import scala.concurrent.duration.Duration

import edu.colorado.fitzgero.sotestbed.config.algorithm.{BatchingFunctionConfig, CombineFlowsFunctionConfig, KSPAlgorithmConfig, MarginalCostFunctionConfig, PathToMarginalFlowsFunctionConfig, SelectionAlgorithmConfig}
import edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig.MATSimConfig.IO.ScenarioData
import edu.colorado.fitzgero.sotestbed.model.numeric.{SimTime, TravelTimeSeconds}

final case class MATSimConfig(
  io: MATSimConfig.IO,
  run: MATSimConfig.Run,
  routing: MATSimConfig.Routing,
  algorithm: MATSimConfig.Algorithm
) {

  /**
    * converts this scenario to a selfish experiment (no system optimal agents)
    * @return updated Run configuration
    */
  def toSelfishExperiment: MATSimConfig = ???

}

object MATSimConfig {

  final case class Run(
    startOfSimTime: SimTime,
    endOfSimTime: SimTime,
    endOfRoutingTime: SimTime,
    matsimStepSize: SimTime,
    matsimSemaphoreTimeoutMs: Long,
    simulationTailTimeout: Duration
  ) {
    require(matsimSemaphoreTimeoutMs > 0, "matsimConfig.run.matsimSemaphoreTimeoutMs must be positive")
  }

  final case class Routing(
    batchWindow: SimTime,
    maxPathAssignments: Int,
    reasonableReplanningLeadTime: TravelTimeSeconds,
    minimumReplanningWaitTime: SimTime,
    minimumRemainingRouteTimeForReplanning: TravelTimeSeconds,
    requestUpdateCycle: SimTime,
    selfish: Routing.Selfish
  ) {
    require(requestUpdateCycle > SimTime.Zero, "matsimConfig.routing.requestUpdateCycle needs to be at least 1")
  }

  object Routing {
    sealed trait Selfish {
      def lastIteration: Int
    }
    object Selfish {
      final case class MATSim(
        lastIteration: Int,
        soRoutingIterationCycle: Int,
        soFirstIteration: Boolean, // overrides the iteration cycle for so-routing and simply has it run on iteration 0
      ) extends Selfish {
        require(soRoutingIterationCycle <= lastIteration,
          "matsimConfig.routing.selfish.matsim.soRoutingIterationCycle needs to be less than or equal to lastIteration")
      }
      final case class Dijkstra(
        pathToMarginalFlowsFunction: PathToMarginalFlowsFunctionConfig,
        combineFlowsFunction: CombineFlowsFunctionConfig,
        marginalCostFunction: MarginalCostFunctionConfig,
      ) extends Selfish {
        def lastIteration: Int = 0
      }
    }
  }

  final case class IO(
    matsimNetworkFile: File,
    populationFile   : File,
    matsimConfigFile : File,
    batchName        : String = System.currentTimeMillis.toString,
    scenarioData     : Option[ScenarioData] = None,
    baseDirectory    : Path = Paths.get("/tmp"),
    matsimLogLevel   : String = "INFO",
  ) {
    def batchLoggingDirectory: Path = {
      baseDirectory.resolve(batchName)
    }

    def experimentLoggingDirectory: Path = {
      this.scenarioData match {
        case None => baseDirectory
        case Some(scenarioData) => scenarioData.toTrialPath(baseDirectory, batchName)
      }
    }

    def experimentDirectory: Path = {
      this.scenarioData match {
        case None => baseDirectory.resolve(batchName)
        case Some(scenarioData) => scenarioData.toExperimentPath(baseDirectory, batchName)
      }
    }
  }

  object IO {
    final case class ScenarioData(
      algorithm  : String,
      trialNumber: Int,
      variation  : String = "",
    ) {
      def toTrialName: String = {
        s"$variation-$trialNumber-$algorithm"
      }

      def toVariationPath(basePath: Path, batchName: String): Path =
        basePath.resolve(batchName).resolve(variation)

      def toTrialPath(basePath: Path, batchName: String): Path =
        basePath.resolve(batchName).resolve(variation).resolve(trialNumber.toString)

      def toExperimentPath(basePath: Path, batchName: String): Path =
        basePath.resolve(batchName).resolve(variation).resolve(trialNumber.toString).resolve(algorithm)
    }
  }

  sealed trait Algorithm {
    /**
      * the algorithm name is interpreted from the type of the algorithm (algorithm.type)
      * and the associated file name by the experiment batch runner, in the case of
      * the so-algorithms
      * @return
      */
    def name: String
  }
  object Algorithm {
    final case object Selfish extends Algorithm {
      def name: String = "selfish"
    }

    final case class SystemOptimal(
      name: String,
      kspAlgorithm: KSPAlgorithmConfig,
      selectionAlgorithm: SelectionAlgorithmConfig,
      pathToMarginalFlowsFunction: PathToMarginalFlowsFunctionConfig,
      combineFlowsFunction: CombineFlowsFunctionConfig,
      marginalCostFunction: MarginalCostFunctionConfig,
      batchingFunction: BatchingFunctionConfig
    ) extends Algorithm
  }

}
