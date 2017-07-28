package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems.NumberGrid.SortingHat
import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0011 extends FunSuite {
  val grid1: String =
    """
      |01 08 04 05 22
      |02 05 09 09 11
      |00 09 03 01 04
      |09 00 11 10 01
    """.stripMargin

  val calc: (Long, Long) => Long = (a: Long, b: Long) => a * b
  val calcAcc = 1
  val select: (Long, Long) => Long =  (x: Long, y: Long) => Math.max(x, y)
  val selectAcc = 0

  val hat = new SortingHat[Long](calc, calcAcc, select, selectAcc)
  val grid = NumberGrid(grid1, hat)

  test("value returns value, nextValueInDirection returns value of specified neighbor from specified start") {
    assert(grid.lengthX == 5)
    assert(grid.lengthY == 4)
    intercept[Exception] { grid.value(-1, 0) }
    intercept[Exception] { grid.value(0, -1) }
    intercept[Exception] { grid.value(0, 4) }
    intercept[Exception] { grid.value(5, 0) }
    assert(grid.value(0, 0) == 1)
    assert(grid.value(2, 0) == 4)
    assert(grid.value(0, 2) == 0)
    assert(grid.value(4, 0) == 22)
    assert(grid.value(4, 3) == 1)
    assert(grid.value(0, 3) == 9)
    assert(grid.value(3, 2) == 1)
    assert(grid.value(1, 2) == 9)

    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.N).contains(3))
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.NE).contains(1))
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.E).contains(10))
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.SE).isEmpty)
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.S).isEmpty)
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.SW).isEmpty)
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.W).contains(0))
    assert(grid.nextValueInDirection(2, 3, NumberGrid.Direction.NW).contains(9))

    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.N).contains(22))
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.NE).isEmpty)
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.E).isEmpty)
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.SE).isEmpty)
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.S).contains(4))
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.SW).contains(1))
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.W).contains(9))
    assert(grid.nextValueInDirection(4, 1, NumberGrid.Direction.NW).contains(5))
  }
}
