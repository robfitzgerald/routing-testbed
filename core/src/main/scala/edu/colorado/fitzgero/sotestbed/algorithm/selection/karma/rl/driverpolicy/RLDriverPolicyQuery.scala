package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy

import java.io.InputStream

import cats.effect.IO
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{Path, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import kantan.csv._
import kantan.csv.ops._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.RLDriverPolicyStructure
import edu.colorado.fitzgero.sotestbed.rllib._
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma._
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.MultiAgentRealAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentDiscreteAction
import edu.colorado.fitzgero.sotestbed.rllib.Action.SingleAgentRealAction

case class RLDriverPolicyQuery(host: String, port: Int, episodeId: EpisodeId) {

  /**
    * function that handles communication to the RL server. knowledge of the
    * host, port, and EpisodeId are persistent between calls, but, the state
    * of the simulation varies.
    *
    * @param rl the RL-based driver policy which provides operations for encoding/decoding
    *           the observation and action spaces
    * @param bank the current karma bank state
    * @param activeAgentHistory the current driver agent histories
    * @param roadNetwork the current road network state
    * @param costFunction an edge cost function
    * @param req a request
    * @param paths the path options available to this request
    * @return the effect of collecting a Bid for this agent
    */
  def getBidAction(
    rl: DriverPolicy.RLBasedDriverPolicy,
    bank: Map[String, Karma],
    hist: ActiveAgentHistory,
    rn: RoadNetwork[IO, Coordinate, EdgeBPR],
    cf: EdgeBPR => Cost
  )(
    req: Request,
    paths: List[Path]
  ): IO[Bid] = {
    rl.structure
      .encodeObservation(rn, cf, hist, bank, req, paths)
      .flatMap { obs =>
        PolicyClientOps
          .send(PolicyClientRequest.GetActionRequest(episodeId, obs), host, port)
          .flatMap { response =>
            response match {
              case PolicyClientResponse.GetActionResponse(action) =>
                rl.structure
                  .decodeActionAsBid(action)
                  .map { bidValue => Bid(req, bidValue) }
              case other =>
                val msg = s"GetActionRequest response was not GetActionResponse, found $other"
                IO.raiseError(new Error(msg))
            }
          }
      }

  }
}
