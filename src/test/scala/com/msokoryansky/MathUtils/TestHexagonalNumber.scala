package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestHexagonalNumber extends FunSuite {
  test("hexagonalNumber return nth number in hexagonal series") {
    intercept[Exception] {
      HexagonalNumber.hexagonalNumber(0)
    }
    assert(HexagonalNumber.hexagonalNumber(1) === 1)
    assert(HexagonalNumber.hexagonalNumber(2) === 6)
    assert(HexagonalNumber.hexagonalNumber(3) === 15)
    assert(HexagonalNumber.hexagonalNumber(4) === 28)
    assert(HexagonalNumber.hexagonalNumber(5) === 45)
  }

  test("isHexagonalNumber checks if number is in hexagonal series") {
    assert(!HexagonalNumber.isHexagonalNumber(-6))
    assert(!HexagonalNumber.isHexagonalNumber(-1))
    assert(!HexagonalNumber.isHexagonalNumber(0))
    assert(HexagonalNumber.isHexagonalNumber(1))
    assert(!HexagonalNumber.isHexagonalNumber(2))
    assert(!HexagonalNumber.isHexagonalNumber(3))
    assert(!HexagonalNumber.isHexagonalNumber(4))
    assert(!HexagonalNumber.isHexagonalNumber(5))
    assert(HexagonalNumber.isHexagonalNumber(6))
    assert(!HexagonalNumber.isHexagonalNumber(10))
    assert(HexagonalNumber.isHexagonalNumber(15))
    assert(!HexagonalNumber.isHexagonalNumber(20))
    assert(HexagonalNumber.isHexagonalNumber(28))
    assert(!HexagonalNumber.isHexagonalNumber(40))
    assert(HexagonalNumber.isHexagonalNumber(45))
  }

  test("getHexagonalNumberIndex returns Option of index of specified number in hexagonal series") {
    assert(HexagonalNumber.getHexagonalNumberIndex(-6) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(-1) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(0) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(1) === Some(1))
    assert(HexagonalNumber.getHexagonalNumberIndex(5) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(6) === Some(2))
    assert(HexagonalNumber.getHexagonalNumberIndex(10) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(15) === Some(3))
    assert(HexagonalNumber.getHexagonalNumberIndex(20) === None)
    assert(HexagonalNumber.getHexagonalNumberIndex(45) === Some(5))
  }

  test("hexagonalNumber and getHexagonalNumberIndex should match") {
    val mismatches = for {
      i <- 1 to 10000000
      index = HexagonalNumber.getHexagonalNumberIndex(HexagonalNumber.hexagonalNumber(i)).getOrElse(0)
      if i.toLong != index
    } yield (i, index)
    assert(mismatches.isEmpty)
  }
}
