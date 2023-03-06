package edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.batchexternalities

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.Path
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.NetworkPolicySignal
import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.fairness.JainFairnessMath
import cats.effect.IO
import cats.implicits._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.ActiveAgentHistory
import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import edu.colorado.fitzgero.sotestbed.algorithm.batching.AgentHistory

object BatchExternalitiesOps {

  def getFirstAndLastPathSpur(alts: Map[Request, List[Path]]): IO[List[(Request, Path, Path)]] =
    alts.toList
      .traverse {
        case (req, paths) =>
          for {
            first <- IO.fromOption(paths.headOption)(new Error(s"agent ${req.agent} needs 2 paths, has 0"))
            last  <- IO.fromOption(paths.lastOption)(new Error(s"agent ${req.agent} needs 2 paths, has 0"))
            _ <- IO.fromEither(
              if (first == last) Left(new Error(s"agent ${req.agent} both paths are the same")) else Right(())
            )
          } yield (req, first, last)
      }

  /**
    * computes the cost of the full path the agent has traversed, along with an optional path spur replacing
    * some portion of the (assumed future section of the) path.
    *
    * @param rn the current state of the road network
    * @param history the current active agent history
    * @param agentId the agent id associated with the path
    * @param spurPath an optional spur to replace toward the end of the agent's current path
    * @return the cost of the path with optional spur, the allocation metric of fftt/tt
    */
  def getPathAllocationWithSpur(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    history: ActiveAgentHistory,
    agentId: String,
    spurPath: Path = List.empty
  ): IO[Double] = {
    import edu.colorado.fitzgero.sotestbed.algorithm.selection.karma.rl.driverpolicy.DriverPolicySpaceV2Ops._
    for {
      spurEdgeData <- spurPath.traverse(_.toEdgeDataButRetainCost(rn))
      hist         <- IO.fromEither(history.getAgentHistoryOrError(agentId))
      cost         <- freeFlowOverTravelTimePercent(rn, hist, spurEdgeData)
    } yield cost
  }

  /**
    * if we reassign only this one agent, how fair is it to the rest of the batch?
    *
    * @param agentToAssign
    * @param costs
    * @return
    */
  def fairnessWithReassigningAgent(
    agentToAssign: String,
    costs: List[(Request, Double, Double)]
    // calcFn: BatchExternalitiesMetric
  ): IO[Double] = {
    val (winners, losers) = costs.partition { case (req, _, _) => req.agent == agentToAssign }
    winners match {
      case Nil => IO.raiseError(new Error(s"agent $agentToAssign not found in batch"))
      case (_, winnerUoCost, _) :: _ =>
        val losersCosts = losers.map { case (_, _, losersCurrentCost) => losersCurrentCost }
        val costs       = winnerUoCost +: losersCosts
        // @TODO: this could be parameterized like the BatchExternalitiesMetric but it has a different shape
        IO.fromOption(JainFairnessMath.fairness(costs))(new Error(s"unable to compute fairness"))
    }
  }
}
