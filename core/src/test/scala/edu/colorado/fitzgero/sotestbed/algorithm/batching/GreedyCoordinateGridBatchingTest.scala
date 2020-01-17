package edu.colorado.fitzgero.sotestbed.algorithm.batching

import edu.colorado.fitzgero.sotestbed.SoTestBedBaseTest

class GreedyCoordinateGridBatchingTest extends SoTestBedBaseTest {
  "GreedyCoordinateGridBatching" when {
    "CoordinateGrid" when {
      "called with a set of integer-rounded test values" should {
        "produce the correct ids" in {
          val grid = new CoordinateGrid(
            minX = 500.0, maxX = 800.0, minY = 200, maxY = 350, splitFactor = 3
          )
          grid.xStep should equal (100.0)
          grid.yStep should equal (50.0)
          grid.getGridId(x=603.0, y=260.0) should equal ("1#1")
          grid.getGridId(x=599.0, y=349.0) should equal ("0#2")
          grid.getGridId(x=700.0, y=350.0) should equal ("2#2")
        }
      }
      "called with a set of non-rounded test values" should {
        "produce the correct ids" in {
          val grid = new CoordinateGrid(
            minX = 500.12, maxX = 799.192, minY = 200.01928, maxY = 349.1343, splitFactor = 3
          )

          grid.getGridId(x=603.0, y=260.0) should equal ("1#1")
          grid.getGridId(x=599.0, y=349.0) should equal ("0#2")
          grid.getGridId(x=700.0, y=349.1343) should equal ("2#2")
        }
      }
      "after producing a label" should {
        "be able to produce a coordinate from the label" in {
          val grid = new CoordinateGrid(
            minX = 400.0, maxX = 800.0, minY = 200, maxY = 600, splitFactor = 16
          )
          val id1 = grid.getGridId(x=599.0, y=451.0)
          id1 should equal ("7#10")
          val id2 = grid.getGridId(x=426.0, y=501.0)
          id2 should equal ("1#12")

          grid.getGridCoord(id1) match {
            case None => fail()
            case Some(coord1) =>
              coord1 should equal ("(575.0000,450.0000)")
          }
          grid.getGridCoord(id2) match {
            case None => fail()
            case Some(coord2) =>
              coord2 should equal ("(425.0000,500.0000)")
          }
          grid.getGridCoord("1#17") should equal (None)    // index out of bounds
          grid.getGridCoord("invalid") should equal (None) // not a grid id
        }
      }
    }
  }
}
