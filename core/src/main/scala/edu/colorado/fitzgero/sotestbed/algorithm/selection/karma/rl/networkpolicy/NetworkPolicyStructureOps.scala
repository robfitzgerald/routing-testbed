package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy

import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetworkIO
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.rllib.EpisodeId
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.networkpolicy.NetworkPolicySpace
import edu.colorado.fitzgero.sotestbed.rllib.Observation
import com.typesafe.scalalogging.LazyLogging
import cats.effect.IO

object NetworkPolicyStructureOps extends LazyLogging {

  def encodeTupledObservation(
    episodeId: EpisodeId,
    roadNetwork: RoadNetworkIO,
    zoneLookup: Map[String, List[EdgeId]],
    space: NetworkPolicySpace
  ): IO[Observation.TupledAgentObservation] = {
    space.encodeObservation(roadNetwork, zoneLookup).flatMap {
      case mao: Observation.MultiAgentObservation =>
        val flattened = mao.observation.toList
          .sortBy { case (agentId, _) => agentId }
          .map { case (_, obs) => obs }
        logger.info(f"space generated multiagent observation with ${mao.observation.size} entries")
        logger.info(f"flattened into a tupled observation with ${flattened.length} entries")
        IO.pure(Observation.TupledAgentObservation(flattened))
      case other =>
        IO.raiseError(new Error(s"single agent observation encoding not as expected"))
    }
  }

}
