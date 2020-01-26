package edu.colorado.fitzgero.sotestbed.algorithm.altpaths

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import edu.colorado.fitzgero.sotestbed.model.agent.{Request, RequestClass, TravelMode}
import edu.colorado.fitzgero.sotestbed.model.numeric.Cost
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.EdgeId
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.edge.EdgeBPRCostOps
import edu.colorado.fitzgero.sotestbed.model.roadnetwork.impl.LocalAdjacencyListFlowNetwork

class kSPwLO_SVPTest extends SoTestBedBaseTest {

  "kSPwLO_SVPTest" when {
    "generateAltsForRequest" should {
      "create the 3 alt paths expected in a toy road network" in {

        /*           it's this map:
         *
         *                   2a ->  2b
         *                /            \
         *   src  ->  0  ->  1a ->  1b ->  4  -> dst
         *                \            /
         *                   3a ->  3b
         */


        for {
          network <- LocalAdjacencyListFlowNetwork.fromMATSimXML(TestNetwork.threeAltPathsFile)
          request = Request("test", EdgeId("src->0"), EdgeId("4->dst"), RequestClass.UE, TravelMode.Car)
        } yield {

          val altsAlgResult = kSPwLO_SVP_Algorithm.generateAltsForRequest(
            request,
            network,
            EdgeBPRCostOps.BPRCostFunctionWithBookCoefficients,
            Cost(1.0),
          )

          altsAlgResult.unsafeRunSync() match {
            case None => fail()
            case Some(kSPwLO_SVP_Algorithm.SingleSVPResult(_, alts, pathsSeen)) =>

              // only 3 possible unique paths
              alts.size should equal(3)

              // 6 different vertices should end up in the SVP vertex list:
              // 2a, 2b, 1a, 1b, 3a, and 3b
              pathsSeen.value should equal (6)

              // should have found exactly these 3 paths (no other unique paths exist)
              val altsStrings = {
                for {
                  alt <- alts
                } yield {
                  alt.map { _.edgeId }.mkString(",")
                }
              }.toSet
              altsStrings should contain("src->0,0->3a,3a->3b,3b->4,4->dst")
              altsStrings should contain("src->0,0->2a,2a->2b,2b->4,4->dst")
              altsStrings should contain("src->0,0->1a,1a->1b,1b->4,4->dst")
          }
        }
      }
    }
  }
}
