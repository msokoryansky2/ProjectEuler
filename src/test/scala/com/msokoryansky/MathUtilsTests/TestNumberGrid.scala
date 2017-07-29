package com.msokoryansky.MathUtilsTests

import com.msokoryansky.MathUtils._
import org.scalatest.FunSuite

class TestNumberGrid extends FunSuite {
  val gridBad1: String =
    """
    """.stripMargin

  val gridBad2: String =
    """
      |44 55 66
      |23 23
      |05 66 22
    """.stripMargin

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
  val grid = NumberGrid(grid1)

  test("grid must be non-empty and rectangular") {
    intercept[Exception] {
      NumberGrid(gridBad1)
    }

    intercept[Exception] {
      NumberGrid(gridBad2)
    }
  }

  test("value returns value of specified cell") {
    assert(grid.lengthX == 5)
    assert(grid.lengthY == 4)
    intercept[Exception] {
      grid.value(-1, 0)
    }
    intercept[Exception] {
      grid.value(0, -1)
    }
    intercept[Exception] {
      grid.value(0, 4)
    }
    intercept[Exception] {
      grid.value(5, 0)
    }
    assert(grid.value(0, 0) == 1)
    assert(grid.value(2, 0) == 4)
    assert(grid.value(0, 2) == 0)
    assert(grid.value(4, 0) == 22)
    assert(grid.value(4, 3) == 1)
    assert(grid.value(0, 3) == 9)
    assert(grid.value(3, 2) == 1)
    assert(grid.value(1, 2) == 9)
  }

  test("nextValueInDirection returns value of specified neighbor from specified start") {
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

  test("nextValuesInDirection returns list of values in specified direction") {
    assert(grid.nextValuesInDirection(2, 3, NumberGrid.Direction.N, 0) == List())
    assert(grid.nextValuesInDirection(2, 3, NumberGrid.Direction.N, 1) == List(11))
    assert(grid.nextValuesInDirection(2, 3, NumberGrid.Direction.N, 3) == List(11, 3, 9))
    assert(grid.nextValuesInDirection(2, 3, NumberGrid.Direction.N, 5) == List(11, 3, 9, 4))
  }

  test("bestCellResultInAllDirections returns best available result in all directions for specified cell") {
    assert(grid.bestCellResultInAllDirections(2, 3, 1, hat) === 11)
    assert(grid.bestCellResultInAllDirections(2, 3, 2, hat) === 110)
    assert(grid.bestCellResultInAllDirections(2, 3, 3, hat) === 297)
    assert(grid.bestCellResultInAllDirections(2, 3, 4, hat) === 1188)
    assert(grid.bestCellResultInAllDirections(4, 0, 1, hat) === 22)
    assert(grid.bestCellResultInAllDirections(4, 0, 2, hat) === 242)
    assert(grid.bestCellResultInAllDirections(4, 0, 3, hat) === 968)
    assert(grid.bestCellResultInAllDirections(4, 0, 4, hat) === 3520)
  }

  test("bestGridResultInAllDirections returns best available result across the grid") {
    assert(grid.bestGridResultInAllDirections(1, hat) === 22)
    assert(grid.bestGridResultInAllDirections(2, hat) === 242)
    assert(grid.bestGridResultInAllDirections(3, hat) === 968)
    assert(grid.bestGridResultInAllDirections(4, hat) === 4455)
  }
}
