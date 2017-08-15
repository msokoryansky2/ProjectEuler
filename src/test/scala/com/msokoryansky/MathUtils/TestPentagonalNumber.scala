package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPentagonalNumber extends FunSuite {
  test("pentagonalNumber returns pentagonal number specified by the index") {
    intercept[Exception] {
      PentagonalNumber.pentagonalNumber(0)
    }
    assert(PentagonalNumber.pentagonalNumber(1) === 1)
    assert(PentagonalNumber.pentagonalNumber(2) === 5)
    assert(PentagonalNumber.pentagonalNumber(3) === 12)
    assert(PentagonalNumber.pentagonalNumber(4) === 22)
    assert(PentagonalNumber.pentagonalNumber(5) === 35)
    assert(PentagonalNumber.pentagonalNumber(6) === 51)
    assert(PentagonalNumber.pentagonalNumber(7) === 70)
    assert(PentagonalNumber.pentagonalNumber(8) === 92)
  }

  test("getPentagonalNumberIndex returns Some of specified number's in pentagonal number series or None") {
    assert(PentagonalNumber.getPentagonalNumberIndex(0) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(1) === Some(1))
    assert(PentagonalNumber.getPentagonalNumberIndex(2) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(3) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(4) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(5) === Some(2))
    assert(PentagonalNumber.getPentagonalNumberIndex(10) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(12) === Some(3))
    assert(PentagonalNumber.getPentagonalNumberIndex(50) === None)
    assert(PentagonalNumber.getPentagonalNumberIndex(70) === Some(7))
  }

  test("isPentagonalNumber returns if number is a pentagonal one") {
    assert(!PentagonalNumber.isPentagonalNumber(0))
    assert(PentagonalNumber.isPentagonalNumber(1))
    assert(!PentagonalNumber.isPentagonalNumber(2))
    assert(!PentagonalNumber.isPentagonalNumber(3))
    assert(!PentagonalNumber.isPentagonalNumber(4))
    assert(PentagonalNumber.isPentagonalNumber(5))
    assert(!PentagonalNumber.isPentagonalNumber(10))
    assert(PentagonalNumber.isPentagonalNumber(22))
    assert(!PentagonalNumber.isPentagonalNumber(30))
    assert(!PentagonalNumber.isPentagonalNumber(50))
    assert(PentagonalNumber.isPentagonalNumber(51))
    assert(!PentagonalNumber.isPentagonalNumber(60))
    assert(PentagonalNumber.isPentagonalNumber(70))
  }

  test("pentagonalNumbers returns stream of pentagonal numbers") {
    assert(PentagonalNumber.pentagonalNumbers().head === 1)
    assert(PentagonalNumber.pentagonalNumbers().tail.head === 5)
    assert(PentagonalNumber.pentagonalNumbers(2).head === 5)
    assert(PentagonalNumber.pentagonalNumbers().tail.tail.head === 12)
    assert(PentagonalNumber.pentagonalNumbers().tail.tail.tail.head === 22)
    assert(PentagonalNumber.pentagonalNumbers().tail.tail.tail.tail.head === 35)
    assert(PentagonalNumber.pentagonalNumbers().tail.tail.tail.tail.tail.head === 51)
  }

  test("distanceBetweenPentagonalNumbers returns distance between two pentagonal numbers") {
    assert(PentagonalNumber.distanceBetweenPentagonalNumbers(1, 5) === 34)
    assert(PentagonalNumber.distanceBetweenPentagonalNumbers(5, 1) === 34)
    assert(PentagonalNumber.distanceBetweenPentagonalNumbers(1, 1) === 0)
    assert(PentagonalNumber.distanceBetweenPentagonalNumbers(8, 6) === 41)
  }

  test("precedingPentagonalNumber and succeedingPentagonalNumber return first Pentagonal numbers around target") {
    assert(PentagonalNumber.precedingPentagonalNumber(70) === 7)
    assert(PentagonalNumber.precedingPentagonalNumber(60) === 6)
    assert(PentagonalNumber.succeedingPentagonalNumber(70) === 7)
    assert(PentagonalNumber.succeedingPentagonalNumber(80) === 8)
  }

  test("firstRemotePentagonalNumber returns index of first pentagonal number " +
    "whose successor is more than specified distance away") {
    assert(PentagonalNumber.firstRemotePentagonalNumber(6) === 2)
    assert(PentagonalNumber.firstRemotePentagonalNumber(13) === 5)
  }

  test("firstPentagonalPairWithProperty returns first pentagonal pair with specified property") {
    assert(PentagonalNumber.firstPentagonalPairWithProperty(1,
      (n1, n2) => PentagonalNumber.distanceBetweenPentagonalNumbers(n1, n2) >= 10) === (1, 3))
  }

  test("allPentagonalPairsWithinDistanceWithProperty returns all pentagonal pairs with specified property") {
    assert(PentagonalNumber.allPentagonalPairsWithinDistanceWithProperty(1, 4, 15,
      (n1, n2) => n1 + n2 >= 15).sortWith(_._1 < _._1) ===
      List((2, 3), (2, 4), (3, 4)))
  }
}
