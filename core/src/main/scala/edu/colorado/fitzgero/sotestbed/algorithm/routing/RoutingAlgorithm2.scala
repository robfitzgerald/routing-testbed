package edu.colorado.fitzgero.sotestbed.algorithm.routing

import cats.effect.IO
import cats.implicits._
import io.circe.syntax._

import com.typesafe.scalalogging.LazyLogging
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner
import edu.colorado.fitzgero.sotestbed.algorithm.altpaths.AltPathsAlgorithmRunner.{
  AltPathsAlgorithmResult,
  AltsResultData
}
import edu.colorado.fitzgero.sotestbed.algorithm.batchfilter.BatchFilterFunction
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentBatchData.RouteRequestData
import edu.colorado.fitzgero.sotestbed.algorithm.batching.{BatchingFunction, BatchingManager}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner
import edu.colorado.fitzgero.sotestbed.algorithm.selection.SelectionRunner.{
  SelectionRunnerRequest,
  SelectionRunnerResult
}
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.{Karma, KarmaSelectionAlgorithm}
import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, RunTime, SimTime}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.CongestionThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ExternalRLServer
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.RandomPolicy
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.UserOptimal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig.ScaledProportionalThreshold
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientRequest
import edu.colorado.fitzgero.sotestbed.rllib.PolicyClientResponse
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.rllib.Observation
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignalGenerator
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkPolicySpace

/**
  *
  * @param altPathsAlgorithmRunner an algorithm that computes alt paths for a batch
  * @param batchingFunction applies a means to group requests
  * @param batchFilterFunction removes batches based on a batch filtering heuristic
  * @param selectionRunner combinatorial search
  * @param minBatchSize ignore batches less than this size. this value should be a function of
  *                     the alt paths algorithm "k" and the batch filter function "minSearchSpaceSize" parameters
  */
case class RoutingAlgorithm2(
  altPathsAlgorithmRunner: AltPathsAlgorithmRunner,
  batchingFunction: BatchingFunction,
  batchFilterFunction: BatchFilterFunction,
  selectionRunner: SelectionRunner,
  minBatchSize: Int,
  replanAtSameLink: Boolean
) extends LazyLogging {

  val altsAlgName: String     = altPathsAlgorithmRunner.altPathsAlgorithm.getClass.getSimpleName
  val batchFilterName: String = batchFilterFunction.getClass.getSimpleName
  val selectionName: String   = selectionRunner.selectionAlgorithm.getClass.getSimpleName

  /**
    * performs all steps related to solving SO route plans
    *
    * @param roadNetwork the current road network state
    * @param requests the requests at this time step
    * @param currentSimTime the current time
    * @param batchingManager tracks state of agent data and releases requests which can be routed
    * @return routing algorithm results for each batch
    */
  def runSO(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    requests: List[RouteRequestData],
    currentSimTime: SimTime,
    batchingManager: BatchingManager,
    bank: Map[String, Karma]
  ): IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])] = {

    if (requests.isEmpty) {
      IO.pure((List.empty, bank))
    } else {
      logger.info("beginning SO routing algorithm")

      // REMOVED
      // this was a way of injecting the current agent trip plan into the 0'th index of the
      // alts with the premise that this would be the "best" choice over the true shortest path.
      // maybe that idea makes sense for human drivers who want less replannings, less friction,
      // but that adds a lot of noise, since those paths may be much farther from user-optimal,
      // throwing off the whole game.
      // val currentPathFn: String => IO[Path] =
      // RoutingAlgorithm2.getCurrentPath(batchingManager, roadNetwork, altPathsAlgorithmRunner.costFunction)

      val result: IO[IO[(List[(String, RoutingAlgorithm.Result)], Map[String, Karma])]] = for {
        batchingFunctionStartTime <- IO { System.currentTimeMillis }
        batchingResultOpt         <- batchingFunction.updateBatchingStrategy(roadNetwork, requests, currentSimTime)
        batchingRuntime           <- IO { RunTime(System.currentTimeMillis - batchingFunctionStartTime) }
        _ = logger.info(s"request batching computed via ${batchingFunction.getClass.getSimpleName}")

      } yield {

        batchingResultOpt match {
          case None =>
            logger.info("no viable requests after applying batching function")
            IO.pure((List.empty, bank))
          case Some(BatchingFunction.BatchingResult(routeRequests, zoneLookup)) =>
            // if not replanAtSameLink, dismiss requests where the location
            // (linkid) has not changed since their last replanning event
            // afterward, remove batches where the batch size is less than
            // the configured minBatchSize threshold
            val preFilteredBatches =
              if (replanAtSameLink) routeRequests
              else
                routeRequests.flatMap {
                  case (batchId, reqs) =>
                    reqs.filter { req =>
                      batchingManager.storedHistory.getPreviousReplanning(req.agent) match {
                        case None => true
                        case Some(prevReplanned) =>
                          prevReplanned.request.location != req.location
                      }
                    } match {
                      case Nil => None
                      case requestsAtNewLocations if reqs.lengthCompare(minBatchSize) >= 0 =>
                        Some(batchId -> requestsAtNewLocations)
                      case _ => None
                    }
                }

            // run alts path generator. this also has a few extra features included
            // such as re-populating the result with network speeds when needed, removing
            // looped paths, and re-sorting the result set based on their estimated
            // travel time.
            val altPathsResult: IO[List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]] =
              preFilteredBatches.traverse {
                case (batchId, batch) =>
                  altPathsAlgorithmRunner.run(batchId, batch, batchingManager.storedHistory, roadNetwork)
              }

            // handle the special case of additional context required for running the karma algorithm
            // when working within the constraints of the SelectionAlgorithm trait
            def instantiateSelection(requests: List[SelectionRunnerRequest]) = {
              RoutingAlgorithm2.instantiateSelectionAlgorithm(
                selectionRunner,
                roadNetwork,
                requests,
                batchingManager,
                bank,
                zoneLookup
              )
            }

            for {
              altsStartTime <- IO { System.currentTimeMillis }
              batchAlts     <- altPathsResult
              _             <- IO.pure(logger.info(s"alt paths computed via $altsAlgName"))
              altResultData = AltPathsAlgorithmRunner.logAltsResultData(batchAlts)
              altsRunTime       <- IO { RunTime(System.currentTimeMillis - altsStartTime) }
              batchAltsFiltered <- batchFilterFunction.filter(batchAlts, roadNetwork)
              batchAltsNoSingletons = RoutingAlgorithm2.removeAgentsWithOnePathAlt(batchAltsFiltered)
              _ <- IO.pure { logger.info(s"batch filter function completed via $batchFilterName") }
              // selectionRunnerRequests <- RoutingAlgorithm2.getCurrentPaths(batchAltsFiltered, currentPathFn)
              selectionRunnerRequests = batchAltsNoSingletons.map { b =>
                val selectionAlts = b.filteredAlts.getOrElse(b.alts)
                SelectionRunner.SelectionRunnerRequest(b.batchId, selectionAlts)
              }
              selectionRunnerFixed <- instantiateSelection(selectionRunnerRequests)
              selectionOutput <- RoutingAlgorithm2.runSelectionWithBank(
                selectionRunnerRequests,
                roadNetwork,
                selectionRunnerFixed,
                bank
              )
              (soResults, updatedBank) = selectionOutput
              _ <- IO.pure(logger.info(s"selection algorithm completed via $selectionName"))
            } yield {
              // re-combine data by batch id and package as a RoutingAlgorithm.Result
              val matched = RoutingAlgorithm2.matchAltBatchesWithSelectionBatches(
                batchAltsFiltered,
                soResults,
                batchingManager,
                altsRunTime,
                batchingRuntime,
                altResultData
              )
              (matched, updatedBank)
            }
        }
      }

      result.flatten
    }
  }
}

object RoutingAlgorithm2 {

  /**
    * constructor which computes the minBatchSize
    *
    * @param altPathsAlgorithmRunner an algorithm that computes alt paths for a batch
    * @param batchingFunction applies a means to group requests
    * @param batchFilterFunction removes batches based on a batch filtering heuristic
    * @param selectionRunner combinatorial search
    * @param k number of alt paths as a parameter for the alt paths runner
//    * @param minSearchSpaceSize ignore batches which cannot produce at least this many combinations

    * @return the Routing Algorithm, v2
    */
  def apply(
    altPathsAlgorithmRunner: AltPathsAlgorithmRunner,
    batchingFunction: BatchingFunction,
    batchFilterFunction: BatchFilterFunction,
    selectionRunner: SelectionRunner,
    k: Int,
    minBatchSize: Int,
    replanAtSameLink: Boolean
//    minSearchSpaceSize: Int
  ): RoutingAlgorithm2 = {
    // 2022-07-01: why was this done? moving back to configuration
    // find log of minSearchSpaceSize in the base of k
//    val minBatchSize: Int = math.ceil(math.log(minSearchSpaceSize.toDouble) / math.log(k)).toInt
    RoutingAlgorithm2(
      altPathsAlgorithmRunner,
      batchingFunction,
      batchFilterFunction,
      selectionRunner,
      minBatchSize,
      replanAtSameLink
    )
  }

  /**
    * adds context to the selection algorithm specific to this time step.
    * this was created to deal with the Karma-based selection algorithm's requirements.
    * @param selectionRunner
    * @param roadNetwork
    * @param selectionRunnerRequest
    * @param batchingManager
    * @param bank
    * @return
    */
  def instantiateSelectionAlgorithm(
    selectionRunner: SelectionRunner,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    selectionRunnerRequest: List[SelectionRunnerRequest],
    batchingManager: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]]
  ): IO[SelectionRunner] = {
    selectionRunner.selectionAlgorithm match {
      case k: KarmaSelectionAlgorithm =>
        // generate a signal for each batch
        val batchesWithSignals: IO[Map[String, NetworkPolicySignal]] = k.networkPolicy match {
          case UserOptimal                                     => IO.pure(Map.empty)
          case ExternalRLServer(underlying, structure, client) =>
            // v2: special handling for Karma-based selection
            // that integrates with an external control module when generating
            // network policy signals

            // we assume here that everything is correctly configured so that batching
            // was informed by the NetworkPolicySpace and therefore ZoneIds match.
            for {
              epId  <- IO.fromOption(k.multiAgentNetworkPolicyEpisodeId)(new Error("missing episode id"))
              space <- IO.fromOption(underlying.space)(new Error("network policy has no 'space'"))
              // log reward since last time step
              lastBatch = k.getPreviousBatch()
              _ <- if (lastBatch.isEmpty) IO.pure(())
              else
                for {
                  req <- structure.generateLogReturnsRequest(epId, roadNetwork, lastBatch, space)
                  _   <- client.sendOne(req)
                  _ = k.networkClientPw.write(req.toBase.asJson.noSpaces.toString + "\n")
                } yield ()
              // get action for this time step
              actReq <- structure.generateGetActionRequest(epId, roadNetwork, zoneLookup, space)
              actRes <- client.sendOne(actReq)
              _ = k.networkClientPw.write(actReq.toBase.asJson.noSpaces.toString + "\n")
              _ = k.networkClientPw.write(actRes.asJson.noSpaces.toString + "\n")
              act  <- actRes.getAction
              sigs <- structure.extractActions(act, space, k.gen, lastBatch.keys.toList)
              // record zone batches to log rewards for at next time step
              // this doesn't change from timestep to timestep for now, but it may in the future
              sigIdLookup   = sigs.keySet
              batchesUpdate = zoneLookup.filter { case (k, _) => sigIdLookup.contains(k) }
              _             = k.updatePreviousBatch(batchesUpdate)
            } yield sigs

          case otherPolicy =>
            val spaceResult: IO[NetworkPolicySpace] =
              IO.fromOption(otherPolicy.space) {
                val msg = s"internal error: policy $otherPolicy expected to have a NetworkPolicySpace"
                new Error(msg)
              }

            for {
              space <- spaceResult
              obs   <- space.encodeObservation(roadNetwork, zoneLookup)
              sigs  <- multiAgentObsToSignals(obs, k.gen)
            } yield sigs

        }
        // pass the generated batch data to the inner Karma algorithm
        batchesWithSignals.map { batches =>
          val signals = batches
          val fixedKarmaSelection = k.build(
            batchingManager.storedHistory,
            signals,
            k.selectionPw,
            k.networkPw,
            k.driverClientPw
          )

          selectionRunner.copy(selectionAlgorithm = fixedKarmaSelection)
        }
      case _ => IO.pure(selectionRunner)
    }
  }

  def multiAgentObsToSignals(
    obs: Observation,
    npsg: NetworkPolicySignalGenerator
  ): IO[Map[String, NetworkPolicySignal]] = {
    obs match {
      case sao: Observation.SingleAgentObservation =>
        IO.raiseError(new Error(s"expected multiagent observation, found $sao"))
      case tao: Observation.TupledAgentObservation =>
        IO.raiseError(new Error(s"expected multiagent observation, found $tao"))
      case Observation.MultiAgentObservation(observation) =>
        observation.toList
          .traverse {
            case (agentId, obsFeatures) =>
              // todo:
              // - this code only handles single-featured network observations, but this isn't future-proof
              val sigOpt = obsFeatures.headOption.map { o => (agentId.value, npsg.generateSignal(o)) }
              IO.fromOption(sigOpt)(new Error(s"expected single observation feature but was empty"))
          }
          .map { _.toMap }
      case Observation.GroupedMultiAgentObservation(observation) =>
        IO.raiseError(new NotImplementedError)
    }
  }

  /**
    * runs the selection algorithm for each selection request, holding aside the
    * effect of interacting with the bank between each run of the selection algorithm.
    *
    * @param requests sub-batch of request data for a selection algorithm
    * @param roadNetwork road network state
    * @param runner algorithm to apply
    * @param bank ledger containing current balances for all agents
    * @return the effect of running this time step of selection algorithms
    */
  def runSelectionWithBank(
    requests: List[SelectionRunnerRequest],
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    runner: SelectionRunner,
    bank: Map[String, Karma]
  ): IO[(List[Option[SelectionRunnerResult]], Map[String, Karma])] = {
    val initial =
      IO.pure((List.empty[Option[SelectionRunnerResult]], bank))
    requests.foldLeft(initial) { (acc, r) =>
      acc.flatMap {
        case (results, innerBank) =>
          runner.run(r, roadNetwork, innerBank).map {
            case None                             => (None +: results, innerBank)
            case Some((result, updatedInnerBank)) => (Some(result) +: results, updatedInnerBank)
          }
      }
    }
  }

  /**
    * helper function to gather all results back together as a routing result for a batch
    * @param alts all alt path results from the same time step
    * @param selections all selection results from the same time step
    * @param batchingManager the manager which provides lookup data for agent (short-term) histories
    * @return routing algorithm results which fit the requirements of the reporting code
    *         while allowing alts and selection results to be unordered and separated.
    */
  def matchAltBatchesWithSelectionBatches(
    alts: List[AltPathsAlgorithmResult],
    selections: List[Option[SelectionRunner.SelectionRunnerResult]],
    batchingManager: BatchingManager,
    alternatePathsRuntime: RunTime,
    batchingFunctionRuntime: RunTime,
    altsResultData: AltsResultData
  ): List[(String, RoutingAlgorithm.Result)] = {

    val altsLookup = alts.map { a => (a.batchId, a) }.toMap
    val result = for {
      selectionOpt    <- selections
      selectionResult <- selectionOpt
      batchId = selectionResult.batchId
      kspResult <- altsLookup.get(batchId)
    } yield {
      // soo... the selection algorithm may have only been exposed to "filtered alts"
      // so the path that was selected and spit out by the SelectionAlgorithm may not
      // complete the full trip. we need to go back to the ksp result and grab the correct
      // path and inject it into the Response for each agent #hack #ugh #finish-the-phd
      val responsesFixed =
        for {
          res   <- selectionResult.selection.selectedRoutes
          paths <- kspResult.alts.get(res.request)
          path = paths(res.pathIndex)
        } yield res.copy(path = path.map { _.edgeId })

      val routingResult = RoutingAlgorithm.Result(
        kspResult = kspResult.alts,
        filteredKspResult = kspResult.filteredAlts.getOrElse(kspResult.alts),
        responses = responsesFixed,
        agentHistory = batchingManager.storedHistory,
        kspRuntime = alternatePathsRuntime,
        batchingRuntime = batchingFunctionRuntime,
        selectionRuntime = selectionResult.runtime,
        altsResultData = altsResultData,
        selfishCost = selectionResult.selection.selfishCost,
        optimalCost = selectionResult.selection.estimatedCost,
        travelTimeDiff = selectionResult.selection.travelTimeDiff,
        meanTravelTimeDiff = selectionResult.selection.averageTravelTimeDiff,
        samples = selectionResult.selection.samples.value
      )
      (batchId, routingResult)
    }
    result
  }

  /**
    * grab the current path for this agentId, using the current road network state.
    *
    * @param batchingManager stores historical path data for active agents
    * @param roadNetwork the current road network state
    * @param costFunction edge cost function
    * @param agentId the agent id to find their current path
    * @return the requested path inside an effect type
    */
  def getCurrentPath(
    batchingManager: BatchingManager,
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    costFunction: EdgeBPR => Cost
  )(agentId: String): IO[List[PathSegment]] = {
    IO.fromEither(batchingManager.storedHistory.getNewestDataOrError(agentId))
      .flatMap { mostRecent =>
        val inner = for {
          edges <- roadNetwork.edges(mostRecent.remainingRoute.map(_.edgeId))
        } yield {
          edges.map(e => PathSegment(e.edgeId, costFunction(e.attribute)))
        }
        inner
      }
  }

  /**
    * for all batches, construct SelectionRunner request objects
    *
    * @param batchAltsFiltered the final set of alt paths we are considering (for the final set of
    *                          batches we are considering)
    * @param currentPathFn a lookup function that gets the most recent path data for an agent
    * @return a list of selection runner requests wrapped in the effect of calling the road network
    */
  def getCurrentPaths(
    batchAltsFiltered: List[AltPathsAlgorithmResult],
    currentPathFn: String => IO[Path]
  ): IO[List[SelectionRunner.SelectionRunnerRequest]] = {
    batchAltsFiltered.traverse { b =>
      b.alts.toList
        .traverse { case (r, alts) => currentPathFn(r.agent).map { p => (r, p +: alts) } }
        .map { paths => SelectionRunner.SelectionRunnerRequest(b.batchId, paths.toMap) }
    }
  }

  /**
    * remove agents from batches who only had one path in their alts set.
    * if that reduces the size of a batch to zero or one, then it is no
    * longer a feasible batch, so we remove the batch.
    *
    * @param ks the ksp result
    * @return updated ksp result with agents without alternatives removed along
    * with any batches that end up too small for SO routing
    */
  def removeAgentsWithOnePathAlt(
    ks: List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult]
  ): List[AltPathsAlgorithmRunner.AltPathsAlgorithmResult] = {
    ks.flatMap { kspResult =>
      // only keep agents with more than one alternative path
      val filterFn     = (req: Request, paths: List[Path]) => paths.size > 1
      val alts         = kspResult.alts.filter(filterFn.tupled)
      val filteredAlts = kspResult.filteredAlts.map { _.filter(filterFn.tupled) }
      // only keep batches with more than 1 agent after the above filter
      if (alts.size < 2 && filteredAlts.exists { _.size < 2 }) {
        None
      } else {
        val updated = kspResult.copy(alts = alts, filteredAlts = filteredAlts)
        Some(updated)
      }
    }
  }
}
