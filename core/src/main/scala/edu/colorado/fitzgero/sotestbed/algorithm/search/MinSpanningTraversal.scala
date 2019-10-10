package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.RoadNetwork.EdgeTriplet
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, PathSegment, VertexId}

/**
  *
  * @param traversalEdgeTriplet
  * @param traversalVertex looked up during traversals (dst vertex for forward trees, src vertex for reverse trees)
  * @param cost
  * @tparam E
  */
case class MinSpanningTraversal[E](traversalEdgeTriplet: Option[EdgeTriplet[E]], traversalVertex: VertexId, cost: Cost) {
  def toPathSegment: Option[PathSegment] = traversalEdgeTriplet.map{ edgeTriplet => PathSegment(edgeTriplet.edgeId, cost) }

  override def toString: String = {
    val edgeTripletString: String = traversalEdgeTriplet match {
      case Some(edgeTriplet) => edgeTriplet.toString
      case None => "~"
    }
    s"v $traversalVertex: $edgeTripletString has path cost $cost"
  }
}