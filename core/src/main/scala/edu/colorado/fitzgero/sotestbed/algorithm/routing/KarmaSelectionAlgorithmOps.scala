package edu.colorado.fitzgero.sotestbed.algorithm.routing

import edu.colorado.fitzgero.sotestbed.algorithm.selection._
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.algorithm.batching.BatchingManager
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.Karma
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.KarmaSelectionAlgorithm
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicyConfig._
import cats.effect.IO
import cats.implicits._
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkPolicySpace
import edu.colorado.fitzgero.sotestbed.rllib.Observation
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignalGenerator
import edu.colorado.fitzgero.sotestbed.model.numeric.SimTime
import edu.colorado.fitzgero.sotestbed.algorithm.selection.chokepoints.ChokePointsHeuristic

object KarmaSelectionAlgorithmOps {

  /**
    * adds context to the selection algorithm specific to this time step.
    * this was created to deal with the Karma and RL-based selection algorithm requirements.
    * @param selectionRunner
    * @param roadNetwork
    * @param selectionRunnerRequest
    * @param batchingManager
    * @param bank
    * @return
    */
  def instantiateSelectionAlgorithm(
    selectionRunner: SelectionRunner,
    currentSimTime: SimTime,
    batchWindow: SimTime,
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    bm: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]]
  ): IO[SelectionRunner] = {
    selectionRunner.selectionAlgorithm match {
      case c: ChokePointsHeuristic =>
        // hack to provide agent experiences
        IO.pure(selectionRunner.copy(selectionAlgorithm = c.build(bm.storedHistory)))
      case k: KarmaSelectionAlgorithm =>
        // generate a signal for each batch
        val batchesWithSignals: IO[Map[String, NetworkPolicySignal]] = k.networkPolicy match {
          case UserOptimal           => IO.pure(Map.empty)
          case ext: ExternalRLServer =>
            // update signals if 1) first time, or 2) duration between steps exceeds window duration
            val updateSignals = k.getLastGetActionTime match {
              case None                    => true
              case Some(lastGetActionTime) => currentSimTime - lastGetActionTime > ext.stepWindowDuration
            }

            if (updateSignals) {
              performNetworkPolicyServerUpdate(k, ext, currentSimTime, rn, bm, bank, zoneLookup)
            } else {
              k.getLastSigs
            }

          case otherPolicy =>
            val spaceResult: IO[NetworkPolicySpace] =
              IO.fromOption(otherPolicy.space) {
                val msg = s"internal error: policy $otherPolicy expected to have a NetworkPolicySpace"
                new Error(msg)
              }

            for {
              space <- spaceResult
              obs   <- space.encodeObservation(rn, zoneLookup)
              sigs  <- multiAgentObsToSignals(obs, k.gen)
            } yield sigs

        }
        // pass the generated batch data to the inner Karma algorithm
        batchesWithSignals.map { batches =>
          val signals = batches
          val fixedKarmaSelection = k.build(
            bm.storedHistory,
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

  /**
    * encapsulates the full effect of
    * 1. rewarding the action from the previous step
    * 2. computing an observation
    * 3. sending the observation to the server, receiving an action
    * 4. decoding the action as a policy signal for each batch
    *
    * @param k karma selection algorithm state at currentSimTime
    * @param ext external rl server configuration
    * @param currentSimTime current simulation time
    * @param roadNetwork current road network state
    * @param batchingManager source of re-routing requests for this batch
    * @param bank current bank state at currentSimTime
    * @param zoneLookup finds zone by edges in the network
    * @return effect of building the network policy signals for these batches
    */
  def performNetworkPolicyServerUpdate(
    k: KarmaSelectionAlgorithm,
    ext: ExternalRLServer,
    currentSimTime: SimTime,
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    batchingManager: BatchingManager,
    bank: Map[String, Karma],
    zoneLookup: Map[String, List[EdgeId]]
  ): IO[Map[String, NetworkPolicySignal]] = {
    for {
      _     <- k.setZoneLookupIfNotSet(zoneLookup) // sorry
      epId  <- IO.fromOption(k.multiAgentNetworkPolicyEpisodeId)(new Error("missing episode id"))
      space <- IO.fromOption(ext.underlying.space)(new Error("network policy has no 'space'"))
      // log reward since last time step (unless this is t_0, then do nothing)
      _ <- if (k.noExistingGetActionQuery) IO.pure(()) // nothing to log returns about
      else
        for {
          req <- ext.structure.generateLogReturnsRequest(epId, roadNetwork, zoneLookup, space)
          _   <- ext.client.sendOne(req)
          _ = k.networkClientPw.write(req.toBase.asJson.noSpaces.toString + "\n")
          _ <- k.logReturnsHandled(currentSimTime)
        } yield ()
      // get action for this time step
      actReq <- ext.structure.generateGetActionRequest(epId, roadNetwork, zoneLookup, space)
      actRes <- ext.client.sendOne(actReq)
      _ = k.networkClientPw.write(actReq.toBase.asJson.noSpaces.toString + "\n")
      _ = k.networkClientPw.write(actRes.asJson.noSpaces.toString + "\n")
      act  <- actRes.getAction
      sigs <- ext.structure.extractActions(act, space, k.gen, zoneLookup.keys.toList)
      _    <- k.setLastGetActionTime(currentSimTime)
      // record zone batches to log rewards for at next time step
      // this doesn't change from timestep to timestep for now, but it may in the future
      // sigIdLookup   = sigs.keySet
      // batchesUpdate = zoneLookup.filter { case (k, _) => sigIdLookup.contains(k) }
      // store the signals for use until the next time we perform a policy server update
      _ <- k.setLastSigs(sigs)
    } yield sigs
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
    currentSimTime: SimTime,
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    runner: SelectionRunner,
    bank: Map[String, Karma]
  ): IO[(List[Option[SelectionRunnerResult]], Map[String, Karma])] = {
    val initial =
      IO.pure((List.empty[Option[SelectionRunnerResult]], bank))
    requests.foldLeft(initial) { (acc, r) =>
      acc.flatMap {
        case (results, innerBank) =>
          runner.run(r, currentSimTime, roadNetwork, innerBank).map {
            case None                             => (None +: results, innerBank)
            case Some((result, updatedInnerBank)) => (Some(result) +: results, updatedInnerBank)
          }
      }
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

}
