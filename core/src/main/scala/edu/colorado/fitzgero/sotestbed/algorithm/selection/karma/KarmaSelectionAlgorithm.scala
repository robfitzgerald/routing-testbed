package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.PrintWriter

import cats.effect.IO
import cats.implicits._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.{SelectionAlgorithm, TrueShortestSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.CongestionObservationType.CongestionObservationResult
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm.karmaLogHeader
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.config.{BankConfig, FreeFlowCostFunctionConfig}
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

case class KarmaSelectionAlgorithm(
  driverPolicy: DriverPolicy,
  networkPolicy: NetworkPolicyConfig,
  auctionPolicy: AuctionPolicy,
  congestionObservation: CongestionObservationType,
  bankConfig: BankConfig,
  freeFlowCostFunction: FreeFlowCostFunctionConfig,
  marginalCostFunction: EdgeBPR => Flow => Cost,
  seed: Option[Long],
  experimentDirectory: java.nio.file.Path
) extends SelectionAlgorithm
    with LazyLogging {

  val selectionLogFile: java.nio.file.Path = experimentDirectory.resolve("karma_log.csv")
  val networkLogFile: java.nio.file.Path   = experimentDirectory.resolve("karma_network_log.csv")
  val selectionPw: PrintWriter             = new PrintWriter(selectionLogFile.toFile)
  val networkPw: PrintWriter               = new PrintWriter(networkLogFile.toFile)

  val networkLogHeader: String = List(
    "batchId",
    networkPolicy.logHeader,
    NetworkPolicySignal.getLogHeader(networkPolicy)
  ).mkString(",")

  selectionPw.write(karmaLogHeader + "\n")
  networkPw.write(networkLogHeader + "\n")

  val gen: NetworkPolicySignalGenerator = networkPolicy.buildGenerator

  /**
    * this close method is used as it is called in RoutingExperiment2's .close() method
    */
  def close(): Unit = {
    selectionPw.close()
    networkPw.close()
  }

  /**
    * sorry...
    * @deprecated call build first, then use the resulting class to solve selection
    */
  def selectRoutes(
    batchId: String,
    alts: Map[Request, List[Path]],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    bank: Map[String, Karma],
    pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
    combineFlowsFunction: Iterable[Flow] => Flow,
    ignoreCostFunction: EdgeBPR => Flow => Cost
  ): IO[SelectionAlgorithm.SelectionAlgorithmResult] =
    IO.raiseError(new Error("cannot call selectRoutes before calling KarmaSelectionAlgorithm.build()"))

  /**
    * builds a selection algorithm for karma problems with the current simulation state
    * @param activeAgentHistory current active agent history
    * @param networkPolicySignals signal from the NetworkPolicy
    * @return a selection algortihm for karma-based problems
    */
  def build(
    activeAgentHistory: ActiveAgentHistory,
    networkObservations: Map[String, CongestionObservationResult],
    networkPolicySignals: Map[String, NetworkPolicySignal],
    selectionLog: PrintWriter,
    networkLog: PrintWriter
  ): SelectionAlgorithm = new SelectionAlgorithm {

    import KarmaSelectionAlgorithm._

    def selectRoutes(
      batchId: String,
      alts: Map[Request, List[Path]],
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      bank: Map[String, Karma],
      pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      ignoreMeToo: EdgeBPR => Flow => Cost
    ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

      if (alts.isEmpty) IO {
        SelectionAlgorithm.SelectionAlgorithmResult(updatedBank = bank)
      }
      else if (alts.size == 1) {

        TrueShortestSelectionAlgorithm().selectRoutes(
          "user-optimal",
          alts,
          roadNetwork,
          bank,
          pathToMarginalFlowsFunction,
          combineFlowsFunction,
          marginalCostFunction
        )
      } else {
        val costFunction = (e: EdgeBPR) => marginalCostFunction(e)(Flow.Zero)
        val collabCostFn =
          (paths: List[Path]) =>
            SelectionAlgorithm.evaluateCostOfSelection(
              paths,
              roadNetwork,
              pathToMarginalFlowsFunction,
              combineFlowsFunction,
              marginalCostFunction
            )

        // run the driver policy and network policy, and use the result to select
        // a path for each driver agent
        // get the costs associated with the trips
        val result = for {
          bids   <- driverPolicy.applyDriverPolicy(alts, bank, activeAgentHistory, roadNetwork, costFunction)
          signal <- IO.fromEither(networkPolicySignals.getOrError(batchId))
          selections = signal.assign(bids, alts)
          paths      = selections.map { case (_, _, path) => path }
          routesUo   = alts.values.flatMap(_.headOption).toList
          costsUo     <- collabCostFn(routesUo)
          costsSo     <- collabCostFn(paths)
          updatedBank <- IO.fromEither(auctionPolicy.resolveAuction(selections, bank, bankConfig.max))
        } yield {
          // construct the responses
          val responses = selections.zip(costsSo.agentPathCosts).map {
            case ((bid, index, path), cost) =>
              val edgeList = path.map {
                _.edgeId
              }
              Response(bid.request, index, edgeList, cost)
          }

          // some stuff for logging
          val selfishCost = costsUo.overallCost
          val optimalCost = costsSo.overallCost
          val avgAlts: Double =
            if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
          val travelTimeDiff: Cost     = optimalCost - selfishCost
          val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)
          val (assignedUo, assignedSo) = selections.partition { case (_, idx, _) => idx == 0 }
          logger.info(f"BATCH $batchId")
          logger.info(f"KARMA - NETWORK SIGNAL $signal")
          logger.info(f"BIDS - ${bids.mkString("[", ",", "]")}")
          logger.info(f"ROUTES - ${assignedUo.length} UO | ${assignedSo.length} SO")
          logger.info(f"AGENTS: ${responses.length} AVG_ALTS: $avgAlts%.2f SAMPLES: 1")
          logger.info(
            f"COST_EST: BEST $optimalCost, SELFISH $selfishCost, " +
              f"DIFF ${travelTimeDiff.value}%.2f AVG_DIFF ${meanTravelTimeDiff.value}%.2f"
          )

          // responses and analytics
          val selectionAlgorithmResult = SelectionAlgorithm.SelectionAlgorithmResult(
            selectedRoutes = responses,
            estimatedCost = optimalCost,
            selfishCost = selfishCost,
            travelTimeDiff = travelTimeDiff,
            averageTravelTimeDiff = meanTravelTimeDiff,
            samples = NonNegativeNumber.One,
            updatedBank = updatedBank,
            ratioOfSearchSpaceExplored = 0.0
          )

          // log info about the karma selection process
          val bidLookup = bids.map { b => b.request.agent -> b }.toMap
          val loggingOrError = responses.traverse { response =>
            val agent = response.request.agent
            for {
              bid          <- bidLookup.getOrError(agent)
              startBalance <- bank.getOrError(agent)
              endBalance   <- updatedBank.getOrError(agent)
            } yield {
              val selectionRow = KarmaSelectionLogRow(
                batchId = batchId,
                agentId = agent,
                startBalance = startBalance,
                endBalance = endBalance,
                bidValue = bid.value,
                route = response.pathIndex
              )

              selectionRow
            }
          }

          val innerResult = loggingOrError.map { logDataList =>
            // log karma data for each agent
            logDataList.foreach { selectionRow => selectionLog.write(selectionRow.toLogRow + "\n") }

            // log the network policy for this batch
            val networkRow = List(batchId, networkPolicy.getLogData, signal.getLogData).mkString(",")
            networkLog.write(networkRow + "\n")

            selectionLog.flush()
            networkLog.flush()

            selectionAlgorithmResult
          }

          IO.fromEither(innerResult)
        }

        result.flatten
      }
    }
  }
}

object KarmaSelectionAlgorithm {

  final case class KarmaBatchData(batchId: String, obs: CongestionObservationResult, signal: NetworkPolicySignal)

  val karmaLogHeader: String = "batchId,agentId,startKarma,endKarma,bidValue,selectedRoute"

  case class KarmaSelectionLogRow(
    batchId: String,
    agentId: String,
    startBalance: Karma,
    endBalance: Karma,
    bidValue: Karma,
    route: Int
  ) {

    def toLogRow: String =
      s"$batchId,$agentId,$startBalance,$endBalance,$bidValue,$route"
  }
}
