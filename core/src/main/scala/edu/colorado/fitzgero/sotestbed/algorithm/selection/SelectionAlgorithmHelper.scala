package edu.colorado.fitzgero.sotestbed.algorithm.selection

import cats.effect.IO

import edu.colorado.fitzgero.sotestbed.model.agent.Request
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, Path, PathSegment, RoadNetwork}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPR
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork.Coordinate
import cats.implicits._

import edu.colorado.fitzgero.sotestbed.model.numeric.{Cost, Flow}

case class SelectionAlgorithmHelper(
  edgesInternal: Array[EdgeBPR],
  mapping: Map[EdgeId, Int],
  choices: Array[Int],
  requestsLookup: Array[Request],
  pathsLookup: Array[Array[Path]],
  altsInternal: Array[Array[Array[Int]]],
  internalRowLength: Int
) {

  /**
    * calculates the marginal cost of the provided selection
    * @param selection array indices as requests, array values indicating the alternative path index
    * @param marginalCostFunction a function which computes the travel time based on the marginal cost of adding flows
    * @return the marginal cost
    */
  def batchMarginalCost(selection: Array[Int], marginalCostFunction: EdgeBPR => Flow => Cost): Double = {
    val rows: Seq[Double] = for {
      i <- 0 until internalRowLength
    } yield {
      val count: Int = selection.indices.foldLeft(0) { (sum, req) => sum + this.altsInternal(req)(selection(req))(i) }
      val result = if (count == 0) {
        0.0
      } else {
        val marginalCost =
          marginalCostFunction(edgesInternal(i))(Flow(count)) - marginalCostFunction(edgesInternal(i))(Flow.Zero)
        marginalCost.value
      }
      result
    }
    rows.sum
  }

  /**
    * given a selection, count the number of occurrences for each agent on each link, using
    * the internal edge index.
    *
    * @param selection selection algorithm representation (a search state)
    * @return for each edge (index), the count of drivers that cross this edge based on this selection
    */
  def edgeCounts(selection: Array[Int]): IndexedSeq[Int] = {
    val edgeCounts: IndexedSeq[Int] = for {
      i <- 0 until internalRowLength
    } yield {
      val count: Int = selection.indices.foldLeft(0) { (sum, req) => sum + this.altsInternal(req)(selection(req))(i) }
      count
    }
    edgeCounts
  }

  /**
    * for each request in a selection, the marginal cost (upper-bound) for the selected route
    * accounting for the non-linear costs due to multiple agents crossing a single edge.
    *
    * @param selection selection algorithm representation (a search state)
    * @param marginalCostFunction a function which computes the travel time based on the marginal cost of adding flows
    * @return for each request (index), the agent's upper-bounded marginal cost
    */
  def agentMarginalCosts(selection: Array[Int], marginalCostFunction: EdgeBPR => Flow => Cost): Array[Double] = {
    val counts = edgeCounts(selection)
    for {
      (pathIdx, reqIdx) <- selection.zipWithIndex
    } yield {
      val path     = this.pathsLookup(reqIdx)(pathIdx)
      val edgeIdxs = path.flatMap { ps => this.mapping.get(ps.edgeId) }
      val cost = edgeIdxs.foldLeft(0.0) { (acc, edgeIdx) =>
        val count = counts(edgeIdx)
        val marginalCost =
          marginalCostFunction(edgesInternal(edgeIdx))(Flow(count)) - marginalCostFunction(edgesInternal(edgeIdx))(
            Flow.Zero
          )
        acc + marginalCost.value
      }
      cost
    }
  }

}

object SelectionAlgorithmHelper {

  /**
    * builds the helper data structures to support quick lookup of EdgeBPR attributes
    * by the selection cost function
    * @param roadNetwork the road network state
    * @param alts alternative paths sets
    * @param agentOrdering an ordering for the agents which is consistent with the selection + traversal policies
    * @param pathOrdering an ordering for the path alternatives which is consistent with the selection policy
    * @return the lookup data structures
    */
  def apply(
    roadNetwork: RoadNetwork[IO, Coordinate, EdgeBPR],
    alts: Map[Request, List[Path]],
    agentOrdering: Option[Ordering[(Request, List[Path])]],
    pathOrdering: Option[Ordering[Path]]
  ): IO[SelectionAlgorithmHelper] = {

    // list of all unique edge ids that appear in these alts,
    // pointing to a new array index position
    val mapping = alts.values.flatten.flatten.toList.distinct.map {
      _.edgeId
    }.zipWithIndex

    val mappingLength = mapping.length

    val edgeBuilder = mapping.traverse {
      case (edgeId, _) =>
        roadNetwork.edge(edgeId).flatMap {
          case None =>
            IO.raiseError(new Error(s"edge $edgeId not found in road network"))
          case Some(edgeIdAndAttr) =>
            IO { edgeIdAndAttr.attribute }
        }
    }

    for {
      edges <- edgeBuilder
    } yield {
      val mappingLookup = mapping.toMap

      // sort according to the traversal policy, if provided
      val (reqsLookup, altsArray) = agentOrdering match {
        case Some(ordering) => alts.toList.sorted(ordering).unzip
        case None           => alts.toList.unzip
      }

      // sort according to the selection policy, if provided
      val pathsLookup = pathOrdering match {
        case Some(ordering) => altsArray.map { _.toArray.sorted(ordering) }.toArray
        case None           => altsArray.map { _.toArray }.toArray
      }

      // agent -> alt -> edge idx, dense representation
      val altsInternal = altsArray.map {
        _.toArray.map { alt =>
          val path: List[PathSegment] = alt
          path.foldLeft(Array.fill(mappingLength)(0)) { (row, seg) =>
            mappingLookup.get(seg.edgeId) match {
              case None      => row
              case Some(idx) => row.updated(idx, 1)
            }
          }
        }
      }.toArray

      val choices = altsInternal.map { _.length }

      val result = SelectionAlgorithmHelper(
        edges.toArray,
        mappingLookup,
        choices,
        reqsLookup.toArray,
        pathsLookup,
        altsInternal,
        mappingLength
      )

      result
    }
  }
}
