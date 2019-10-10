package edu.colorado.fitzgero.sotestbed.algorithm.search

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPRCostOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{EdgeId, TraverseDirection, VertexId}
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

class SpanningTreeTest extends SoTestBedBaseTest {

  "SpanningTreeTest" when {
    "edgeOrientedSpanningTree" should {
      "produce a forward-oriented min spanning tree" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.threeAltPathsFile)
          root = EdgeId("src->0")
          dst = EdgeId("4->dst")
          result <- SpanningTree.edgeOrientedSpanningTree(network, EdgeBPRCostOps.freeFlowCostFunction, root, dst, TraverseDirection.Forward).unsafeRunSync()
          edges = result.tree.values.flatMap{_.traversalEdgeTriplet}
        } {
          // seven (unique) explored edges
          edges.toSet.size should equal (7)
        }
      }
      "product a reverse-oriented min spanning tree" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.threeAltPathsFile)
          root = EdgeId("4->dst")
          dst = EdgeId("src->0")
          result <- SpanningTree.edgeOrientedSpanningTree(network, EdgeBPRCostOps.freeFlowCostFunction, root, dst, TraverseDirection.Reverse).unsafeRunSync()
          edges = result.tree.values.flatMap{_.traversalEdgeTriplet}
        } {
          edges.foreach(println)
          // seven (unique) explored edges
          edges.toSet.size should equal (7)
        }
      }
    }

    "vertexOrientedMinSpanningTree" ignore {

    }

  }
}
