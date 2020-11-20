package edu.colorado.fitzgero.sotestbed.matsim.config.matsimconfig

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.time.LocalTime

import scala.concurrent.duration.Duration

import cats.Monad

import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import pureconfig._
import pureconfig.configurable._
import pureconfig.ConvertHelpers._
import pureconfig.generic.auto._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{ActiveAgentHistory, BatchingFunction}
import edu.colorado.fitzgero.sotestbed.algorithm.routing.RoutingAlgorithm
import edu.colorado.fitzgero.sotestbed.config.{
  BatchFilterFunctionConfig,
  BatchingFunctionConfig,
  CombineFlowsFunctionConfig,
  EdgeUpdateFunctionConfig,
  KSPAlgorithmConfig,
  KSPFilterFunctionConfig,
  MarginalCostFunctionConfig,
  PathToMarginalFlowsFunctionConfig,
  RoutingReportConfig,
  SelectionAlgorithmConfig
}
import edu.colorado.fitzgero.sotestbed.matsim.model.agent.AgentActivity
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, SimTime, TravelTimeSeconds}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork

final case class MATSimConfig(
  io: MATSimConfig.IO,
  run: MATSimConfig.Run,
  routing: MATSimConfig.Routing,
  population: MATSimConfig.Population,
  algorithm: MATSimConfig.Algorithm
)

object MATSimConfig {

  implicit val localDateConvert: ConfigConvert[LocalTime] = localTimeConfigConvert(AgentActivity.MATSimTextTimeFormat)

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
    adoptionRate: Double,
    maxPathAssignments: Int,
    minimumReplanningLeadTime: TravelTimeSeconds, // broken during a fix of the reporting, would need to route RoadNetwork into batching ops
    minimumReplanningWaitTime: SimTime,           // we ignore replanning for agents which have not traveled at least this long between replanning events
    minimumAverageImprovement: Cost,
    minRequestUpdateThreshold: SimTime, // batching manager state updates
    minNetworkUpdateThreshold: SimTime, // road network state updates
    minBatchSize: Int,
    selfish: Routing.Selfish
  ) {
    require(
      minimumAverageImprovement >= Cost.Zero,
      "matsimConfig.routing.minimumAverageImprovement should be non-negative"
    )
    require(
      minRequestUpdateThreshold >= SimTime.Zero,
      "matsimConfig.routing.minRequestUpdateThreshold should be non-negative"
    )
    require(
      minNetworkUpdateThreshold >= SimTime.Zero,
      "matsimConfig.routing.minNetworkUpdateThreshold should be non-negative"
    )
  }

  object Routing {

    // how will selfish agents be routed
    sealed trait Selfish {
      def lastIteration: Int
    }

    object Selfish {

      final case class MATSim(
        lastIteration: Int,
        soRoutingIterationCycle: Int,
        soFirstIteration: Boolean // overrides the iteration cycle for so-routing and simply has it run on iteration 0
      ) extends Selfish {
        require(
          soRoutingIterationCycle <= lastIteration,
          "matsimConfig.routing.selfish.matsim.soRoutingIterationCycle needs to be less than or equal to lastIteration"
        )
      }

      final case class Dijkstra(
        pathToMarginalFlowsFunction: PathToMarginalFlowsFunctionConfig,
        combineFlowsFunction: CombineFlowsFunctionConfig,
        marginalCostFunction: MarginalCostFunctionConfig
      ) extends Selfish {
        def lastIteration: Int = 0
      }
    }
  }

  final case class IO(
    matsimNetworkFile: File,
    populationPolygonFile: Option[File],
    matsimConfigFile: File,
    routingReportConfig: RoutingReportConfig,
    heatmapLogCycleMinutes: Int = 15,
    heatmapH3Resolution: Int = 9,
    populationFile: File = Paths.get("/tmp/popTempFile.xml").toFile, // overwritten in MATSimBatchExperimentApp
    matsimLogLevel: String = "INFO",
    batchName: String = System.currentTimeMillis.toString,
    outputBaseDirectory: Path = Paths.get("/tmp")
  ) {

    def batchLoggingDirectory: Path = {
      outputBaseDirectory.resolve(batchName)
    }
  }

  final case class Population(
    workActivityMinTime: LocalTime,
    workActivityMaxTime: LocalTime,
    workDurationHours: Int
  )

  sealed trait Algorithm {

    /**
      * the algorithm name is interpreted from the type of the algorithm (algorithm.type)
      * and the associated file name by the experiment batch runner, in the case of
      * the so-algorithms
      * @return
      */
    def name: String
    def selfishOnly: Boolean
    def edgeUpdateFunction: EdgeUpdateFunctionConfig
    def marginalCostFunction: MarginalCostFunctionConfig
  }

  object Algorithm {

    final case class Selfish(
      edgeUpdateFunction: EdgeUpdateFunctionConfig,
      marginalCostFunction: MarginalCostFunctionConfig
    ) extends Algorithm {
      override def name: String         = "selfish"
      override def selfishOnly: Boolean = true

      def build[F[_]: Monad, V, E](): RoutingAlgorithm[F, V, E] = new RoutingAlgorithm[F, V, E] {

        def route(
          requests: List[Request],
          activeAgentHistory: ActiveAgentHistory,
          roadNetwork: RoadNetwork[F, V, E]
        ): F[RoutingAlgorithm.Result] =
          throw new IllegalStateException("algorithm.type is selfish, so we shouldn't be doing any routing.")
      }

      def batchingStub: BatchingFunction = new BatchingFunction {

        /**
          * takes the current batching strategy and any updates about replan-able agents, and spits out an
          * update to that batching strategy
          *
          * @param roadNetwork          the current road network state
          * @param activeRouteRequests agents which are available for SO routing requests0
          * @param currentTime          the current sim time
          * @return an update to the batching strategy, or None if there's nothing to replan (empty list)
          */
        def updateBatchingStrategy[F[_]: Monad, V, E](
          roadNetwork: RoadNetwork[F, V, E],
          activeRouteRequests: List[RouteRequestData],
          currentTime: SimTime
        ): F[Option[List[(String, List[Request])]]] = Monad[F].pure { None }
      }
    }

    final case class SystemOptimal(
      name: String,
      kspAlgorithm: KSPAlgorithmConfig,
      selectionAlgorithm: SelectionAlgorithmConfig,
      edgeUpdateFunction: EdgeUpdateFunctionConfig,
      pathToMarginalFlowsFunction: PathToMarginalFlowsFunctionConfig,
      combineFlowsFunction: CombineFlowsFunctionConfig,
      marginalCostFunction: MarginalCostFunctionConfig,
      batchingFunction: BatchingFunctionConfig,
      batchFilterFunction: BatchFilterFunctionConfig,
      kspFilterFunction: KSPFilterFunctionConfig,
      useFreeFlowNetworkCostsInPathSearch: Boolean
    ) extends Algorithm {
      override def selfishOnly: Boolean = false
    }
  }

}
