package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma

import java.io.PrintWriter

import cats.effect.IO

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm.header
import edu.colorado.fitzgero.sotestbed.config.BankConfig
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, Response}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow, NonNegativeNumber}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, RoadNetwork}

case class KarmaSelectionAlgorithm(
  driverPolicy: DriverPolicy,
  networkPolicy: NetworkPolicy,
  congestionObservation: CongestionObservation,
  bankConfig: BankConfig,
  seed: Option[Long],
  experimentDirectory: java.nio.file.Path
) extends SelectionAlgorithm[IO, Coordinate, EdgeBPR]
    with LazyLogging {

  val logFilePath: java.nio.file.Path = experimentDirectory.resolve("karma_log.csv")
  val pw: PrintWriter                 = new PrintWriter(logFilePath.toFile)
  val logHeader: String               = header(networkPolicy.header, networkPolicy.signalHeader, driverPolicy.header)
  pw.write(logHeader + "\n")

  def close(): Unit = pw.close()

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
    marginalCostFunction: EdgeBPR => Flow => Cost
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
    networkPolicySignals: Map[String, NetworkPolicySignal],
    pw: PrintWriter
  ): SelectionAlgorithm[IO, Coordinate, EdgeBPR] = new SelectionAlgorithm[IO, Coordinate, EdgeBPR] {

    import KarmaSelectionAlgorithm._

    def selectRoutes(
      batchId: String,
      alts: Map[Request, List[Path]],
      roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
      bank: Map[String, Karma],
      pathToMarginalFlowsFunction: (RoadNetwork[IO, Coordinate, EdgeBPR], Path) => IO[List[(EdgeId, Flow)]],
      combineFlowsFunction: Iterable[Flow] => Flow,
      marginalCostFunction: EdgeBPR => Flow => Cost
    ): IO[SelectionAlgorithm.SelectionAlgorithmResult] = {

      // copied in from other selection algorithms, kinda messy
      def costFn =
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
      for {
        bids   <- driverPolicy.applyDriverPolicy(alts.keys.toList, bank, activeAgentHistory)
        signal <- IO.fromOption(networkPolicySignals.get(batchId))(new Error(s"unknown batch id $batchId"))
        selections = signal.assign(bids, alts)
        paths      = selections.map { case (_, _, path) => path }
        costsUo <- costFn(alts.values.flatMap {
          _.headOption
        }.toList)
        costsSo <- costFn(paths)
      } yield {
        // construct the responses
        val responses = selections.zip(costsSo.agentPathCosts).map {
          case ((bid, index, path), cost) =>
            val edgeList = path.map {
              _.edgeId
            }
            Response(bid.request, index, edgeList, cost)
        }
        val updatedBank = KarmaOps.resolveBidsUniformly(bids, bank, bankConfig.max)

        // some stuff for logging
        val selfishCost = costsUo.overallCost
        val optimalCost = costsSo.overallCost
        val avgAlts: Double =
          if (alts.isEmpty) 0d else alts.map { case (_, alts) => alts.size }.sum.toDouble / alts.size
        val travelTimeDiff: Cost     = optimalCost - selfishCost
        val meanTravelTimeDiff: Cost = Cost((optimalCost - selfishCost).value / alts.size)
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

        val bidLookup = bids.map { b => b.request.agent -> b }.toMap
        for {
          response <- responses
        } {
          val bidValue = bidLookup.get(response.request.agent).map { _.value }.getOrElse(Karma(0.0))
          val row = LogRow(
            batchId = batchId,
            agentId = response.request.agent,
            startBalance = bank.getOrElse(response.request.agent, Karma(-1)),
            endBalance = updatedBank.getOrElse(response.request.agent, Karma(-1)),
            congestion = signal.congestion.getOrElse(-1),
            route = response.pathIndex,
            driverCols = bidValue.value.toString,
            networkCols = networkPolicy.getCoefString,
            sigCols = signal.getCoefs
          )
          pw.write(row.toLogRow)
          pw.flush()
        }

        selectionAlgorithmResult
      }
    }
  }
}

object KarmaSelectionAlgorithm {

  def header(networkCols: String, sigCols: String, driverCols: String): String =
    s"batchId,agentId,startKarma,endKarma,congestion,route,$networkCols,$driverCols"

  case class LogRow(
    batchId: String,
    agentId: String,
    startBalance: Karma,
    endBalance: Karma,
    congestion: Double,
    route: Int,
    driverCols: String,
    networkCols: String,
    sigCols: String
  ) {

    def toLogRow: String =
      s"$batchId,$agentId,$startBalance,$endBalance,$congestion,$route,$driverCols,$networkCols,$sigCols\n"
  }
}
