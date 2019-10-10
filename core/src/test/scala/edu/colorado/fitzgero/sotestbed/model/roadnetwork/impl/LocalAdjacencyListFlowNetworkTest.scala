package edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.{TraverseDirection, VertexId}

class LocalAdjacencyListFlowNetworkTest extends SoTestBedBaseTest {

  "LocalAdjacencyListFlowNetworkTest" when {

    "fromMATSimXML" should {
      "produce the correct network" in {
        LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.fiveByFiveNetworkFile) match {
          case Left(error) =>
            fail(error)
          case Right(network) =>
            network.edges.keys.size should equal (80)
            network.vertices.keys.size should equal (25)
        }
      }
    }

    "edgesAndNeighbors" should {
      "work forward" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.fiveByFiveNetworkFile)
          src = VertexId("2")
          result = network.incidentEdgeTriplets(src, TraverseDirection.Forward).unsafeRunSync()
        } yield {
          result.map{t => s"${t.edgeId.value}"}.foreach(println)
        }
      }
      "work reverse" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.fiveByFiveNetworkFile)
          src = VertexId("24")
          result = network.incidentEdgeTriplets(src, TraverseDirection.Reverse).unsafeRunSync()
        } yield {
          result.map{t => s"${t.edgeId.value} ${t.edgeId.value}"}.foreach(println)
        }
      }
    }

  }
}
