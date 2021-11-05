package edu.colorado.fitzgero.sotestbed.matsim.simulator

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest
import org.matsim.api.core.v01.Id
import org.matsim.api.core.v01.network.Link

class MATSimRouteOpsTest extends SoTestBedBaseTest {

  "MATSimRouteOpsTest" when {
    "coalescePath" when {
      "old path is empty" should {
        "return an error" in {
          val currentLinkId = Id.create("i1", classOf[Link])
          val oldPath       = List.empty[Id[Link]]
          val newPath       = List(currentLinkId)
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result.isLeft should be(true)
        }
      }
      "new path is empty when old path is not" should {
        "return an error" in {
          val currentLinkId = Id.create("i1", classOf[Link])
          val oldPath       = List(currentLinkId)
          val newPath       = List.empty[Id[Link]]
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result.isLeft should be(true)
        }
      }
      "old and new path have different end points" should {
        "return an error" in {
          val oldPath       = List("start", "s1", "o2", "o3", "o4", "end").map(Id.create(_, classOf[Link]))
          val newPath       = List("s1", "n2", "n3", "n4", "different ending").map(Id.create(_, classOf[Link]))
          val currentLinkId = Id.create("s1", classOf[Link])
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result.isLeft should be(true)
        }
      }
      "old and new path have same start and end" should {
        "output the entire new path" in {
          val oldPath       = List("start", "s1", "o2", "o3", "o4", "end").map(Id.create(_, classOf[Link]))
          val newPath       = List("start", "s1", "n2", "n3", "n4", "end").map(Id.create(_, classOf[Link]))
          val currentLinkId = Id.create("s1", classOf[Link])
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result match {
            case Left(error) =>
              fail(error)
            case Right(coalescedPath) =>
              coalescedPath should equal(newPath)
          }
        }
      }
      "new path starts somewhere along old path, and current link id is in both paths" should {
        "output a coalesced path" in {
          val oldPath             = List("start", "s1", "s2", "o3", "o4", "end").map(Id.create(_, classOf[Link]))
          val newPath             = List("s2", "n3", "n4", "end").map(Id.create(_, classOf[Link]))
          val validCurrentLinkIds = List("start", "s1", "s2").map(Id.create(_, classOf[Link]))

          val expectedCoalescedPath = List("start", "s1", "s2", "n3", "n4", "end").map(Id.create(_, classOf[Link]))
          for {
            currentLinkId <- validCurrentLinkIds
          } {
            val result = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
            result match {
              case Left(error) =>
                fail(error)
              case Right(coalescedPath) =>
                coalescedPath should equal(expectedCoalescedPath)
            }
          }
        }
      }
      "new path's start point doesn't exist in the old path" should {
        "output an error" in {
          val oldPath       = List("start", "s1", "o2", "o3", "o4", "end").map(Id.create(_, classOf[Link]))
          val newPath       = List("unknown location", "n2", "n3", "n4", "end").map(Id.create(_, classOf[Link]))
          val currentLinkId = Id.create("s1", classOf[Link])
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result.isLeft should be(true)
        }
      }
      "combined old + new path do not contain the current link id" should {
        "output an error" in {
          val oldPath       = List("start", "s1", "current", "o3", "o4", "end").map(Id.create(_, classOf[Link]))
          val newPath       = List("s1", "n2", "n3", "n4", "end").map(Id.create(_, classOf[Link]))
          val currentLinkId = Id.create("current", classOf[Link])
          val result        = MATSimRouteOps.coalescePath(oldPath, newPath, currentLinkId)
          result.isLeft should be(true)
        }
      }
    }
  }
}
