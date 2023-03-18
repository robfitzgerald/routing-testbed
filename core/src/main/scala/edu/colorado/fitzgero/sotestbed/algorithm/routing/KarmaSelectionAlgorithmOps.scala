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
    roadNetwork: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
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
              _     <- k.setZoneLookupIfNotSet(zoneLookup) // sorry
              epId  <- IO.fromOption(k.multiAgentNetworkPolicyEpisodeId)(new Error("missing episode id"))
              space <- IO.fromOption(underlying.space)(new Error("network policy has no 'space'"))
              // log reward since last time step (unless this is t_0, then do nothing)
              _ <- if (k.noExistingGetActionQuery) IO.pure(()) // nothing to log returns about
              else
                for {
                  req <- structure.generateLogReturnsRequest(epId, roadNetwork, zoneLookup, space)
                  _   <- client.sendOne(req)
                  _ = k.networkClientPw.write(req.toBase.asJson.noSpaces.toString + "\n")
                  _ <- k.logReturnsHandled(currentSimTime)
                } yield ()
              // get action for this time step
              actReq <- structure.generateGetActionRequest(epId, roadNetwork, zoneLookup, space)
              actRes <- client.sendOne(actReq)
              _ = k.networkClientPw.write(actReq.toBase.asJson.noSpaces.toString + "\n")
              _ = k.networkClientPw.write(actRes.asJson.noSpaces.toString + "\n")
              act  <- actRes.getAction
              sigs <- structure.extractActions(act, space, k.gen, zoneLookup.keys.toList)
              _    <- k.getActionsSent(currentSimTime)
              // record zone batches to log rewards for at next time step
              // this doesn't change from timestep to timestep for now, but it may in the future
              sigIdLookup   = sigs.keySet
              batchesUpdate = zoneLookup.filter { case (k, _) => sigIdLookup.contains(k) }
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
