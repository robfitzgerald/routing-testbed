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

object AssignmentFairnessExternality {

  /**
    * this metric captures the marginal fairness (- or +) that is the result
    * of assigning a new route for the named agent.
    *
    * example 1: "assigning makes things more fair for this batch" $agent - current > 0$
    * example 2: "assigning makes things less fair for this batch" $agent - current < 0$
    *
    * @param rn current road network state
    * @param alts proposed new path and current path for each agent request
    * @param hists the route request history
    * @param agentId agent to evaluate externalities of new path assignment for
    * @return the diff between with and without assignment for agent $agentId
    */
  def calculate(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    alts: Map[Request, List[Path]],
    hists: ActiveAgentHistory,
    agentId: String
  ): IO[Double] = {
    val replanFn = onlyReplanAgentById(agentId) _
    for {
      current <- fairnessOfAssignment(rn, alts, hists, pickCurrentPaths)
      agent   <- fairnessOfAssignment(rn, alts, hists, replanFn)
    } yield agent - current
  }

  def pickCurrentPaths(alts: Map[Request, List[Path]]): List[(Request, Path)] =
    alts.toList
      .flatMap { case (req, paths) => paths.lastOption.map { c => (req, c) } }

  def onlyReplanAgentById(agentId: String)(alts: Map[Request, List[Path]]): List[(Request, Path)] =
    alts.toList
      .flatMap {
        case (req, paths) =>
          val pathOption = if (req.agent == agentId) paths.headOption else paths.lastOption
          pathOption.map { c => (req, c) }
      }

  /**
    * calculates the fairness of an assignment, picked by applying an assignment function
    *
    * @param rn road network state
    * @param alts path alternatives (new and current) for each agent
    * @param hists the histories of route requests
    * @param fn function used to assign a path to each request
    * @return fairness of the assignment
    */
  def fairnessOfAssignment(
    rn: RoadNetwork[IO, LocalAdjacencyListFlowNetwork.Coordinate, EdgeBPR],
    alts: Map[Request, List[Path]],
    hists: ActiveAgentHistory,
    assignmentFn: Map[Request, List[Path]] => List[(Request, Path)]
  ): IO[Double] = {
    import BatchExternalitiesOps._
    val getAllocFn = (r: Request, p: Path) => getPathAllocationWithSpur(rn, hists, r.agent, p)

    for {
      costs    <- assignmentFn(alts).traverse(getAllocFn.tupled)
      fairness <- IO.fromOption(JainFairnessMath.fairness(costs))(new Error(s"unable to compute fairness"))
    } yield fairness
  }
}
