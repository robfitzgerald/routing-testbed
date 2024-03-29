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
            network.edgesMap.keys.size should equal(80)
            network.verticesMap.keys.size should equal(25)
        }
      }
    }

    "edgesAndNeighbors" should {
      "work forward" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.fiveByFiveNetworkFile)
          src    = VertexId("2")
          result = network.incidentEdgeTriplets(src, TraverseDirection.Forward).unsafeRunSync()
        } yield {
          result
            .map { t =>
              s"${t.edgeId.value}"
            }
            .foreach(println)
        }
      }
      "work reverse" in {
        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.fiveByFiveNetworkFile)
          src    = VertexId("24")
          result = network.incidentEdgeTriplets(src, TraverseDirection.Reverse).unsafeRunSync()
        } yield {
          result
            .map { t =>
              s"${t.edgeId.value} ${t.edgeId.value}"
            }
            .foreach(println)
        }
      }
    }
    "util" in {
      for {
        network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.denver)
      } {
        val minX = network.verticesMap.map { _._2.x }.min
        val minY = network.verticesMap.map { _._2.y }.min
        val maxX = network.verticesMap.map { _._2.x }.max
        val maxY = network.verticesMap.map { _._2.y }.max
        println(f"min x,y: $minX%.4f $minY%.4f")
        println(f"max x,y: $maxX%.4f $maxY%.4f")
        println(f"x range ${maxX - minX}%.4f y range ${maxY - minY}%.4f")
      }
    }
  }
}
