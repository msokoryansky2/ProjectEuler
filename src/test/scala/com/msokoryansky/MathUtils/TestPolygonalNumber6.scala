package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPolygonalNumber6 extends FunSuite {
  test("hexagonalNumber return nth number in hexagonal series") {
    intercept[Exception] {
      PolygonalNumber6.hexagonalNumber(0)
    }
    assert(PolygonalNumber6.hexagonalNumber(1) === 1)
    assert(PolygonalNumber6.hexagonalNumber(2) === 6)
    assert(PolygonalNumber6.hexagonalNumber(3) === 15)
    assert(PolygonalNumber6.hexagonalNumber(4) === 28)
    assert(PolygonalNumber6.hexagonalNumber(5) === 45)
  }

  test("isHexagonalNumber checks if number is in hexagonal series") {
    assert(!PolygonalNumber6.isHexagonalNumber(-6))
    assert(!PolygonalNumber6.isHexagonalNumber(-1))
    assert(!PolygonalNumber6.isHexagonalNumber(0))
    assert(PolygonalNumber6.isHexagonalNumber(1))
    assert(!PolygonalNumber6.isHexagonalNumber(2))
    assert(!PolygonalNumber6.isHexagonalNumber(3))
    assert(!PolygonalNumber6.isHexagonalNumber(4))
    assert(!PolygonalNumber6.isHexagonalNumber(5))
    assert(PolygonalNumber6.isHexagonalNumber(6))
    assert(!PolygonalNumber6.isHexagonalNumber(10))
    assert(PolygonalNumber6.isHexagonalNumber(15))
    assert(!PolygonalNumber6.isHexagonalNumber(20))
    assert(PolygonalNumber6.isHexagonalNumber(28))
    assert(!PolygonalNumber6.isHexagonalNumber(40))
    assert(PolygonalNumber6.isHexagonalNumber(45))
  }

  test("getHexagonalNumberIndex returns Option of index of specified number in hexagonal series") {
    assert(PolygonalNumber6.getHexagonalNumberIndex(-6) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(-1) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(0) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(1) === Some(1))
    assert(PolygonalNumber6.getHexagonalNumberIndex(5) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(6) === Some(2))
    assert(PolygonalNumber6.getHexagonalNumberIndex(10) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(15) === Some(3))
    assert(PolygonalNumber6.getHexagonalNumberIndex(20) === None)
    assert(PolygonalNumber6.getHexagonalNumberIndex(45) === Some(5))
  }

  test("hexagonalNumber and getHexagonalNumberIndex should match") {
    val mismatches = for {
      i <- 1 to 10000000
      index = PolygonalNumber6.getHexagonalNumberIndex(PolygonalNumber6.hexagonalNumber(i)).getOrElse(0)
      if i.toLong != index
    } yield (i, index)
    assert(mismatches.isEmpty)
  }
}
