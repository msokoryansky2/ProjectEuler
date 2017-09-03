package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPolygonalNumber5 extends FunSuite {
  test("pentagonalNumber returns pentagonal number specified by the index") {
    intercept[Exception] {
      PolygonalNumber5.pentagonalNumber(0)
    }
    assert(PolygonalNumber5.pentagonalNumber(1) === 1)
    assert(PolygonalNumber5.pentagonalNumber(2) === 5)
    assert(PolygonalNumber5.pentagonalNumber(3) === 12)
    assert(PolygonalNumber5.pentagonalNumber(4) === 22)
    assert(PolygonalNumber5.pentagonalNumber(5) === 35)
    assert(PolygonalNumber5.pentagonalNumber(6) === 51)
    assert(PolygonalNumber5.pentagonalNumber(7) === 70)
    assert(PolygonalNumber5.pentagonalNumber(8) === 92)
  }

  test("getPentagonalNumberIndex returns Some of specified number's in pentagonal number series or None") {
    assert(PolygonalNumber5.getPentagonalNumberIndex(0) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(1) === Some(1))
    assert(PolygonalNumber5.getPentagonalNumberIndex(2) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(3) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(4) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(5) === Some(2))
    assert(PolygonalNumber5.getPentagonalNumberIndex(10) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(12) === Some(3))
    assert(PolygonalNumber5.getPentagonalNumberIndex(50) === None)
    assert(PolygonalNumber5.getPentagonalNumberIndex(70) === Some(7))
  }

  test("isPentagonalNumber returns if number is a pentagonal one") {
    assert(!PolygonalNumber5.isPentagonalNumber(0))
    assert(PolygonalNumber5.isPentagonalNumber(1))
    assert(!PolygonalNumber5.isPentagonalNumber(2))
    assert(!PolygonalNumber5.isPentagonalNumber(3))
    assert(!PolygonalNumber5.isPentagonalNumber(4))
    assert(PolygonalNumber5.isPentagonalNumber(5))
    assert(!PolygonalNumber5.isPentagonalNumber(10))
    assert(PolygonalNumber5.isPentagonalNumber(22))
    assert(!PolygonalNumber5.isPentagonalNumber(30))
    assert(!PolygonalNumber5.isPentagonalNumber(50))
    assert(PolygonalNumber5.isPentagonalNumber(51))
    assert(!PolygonalNumber5.isPentagonalNumber(60))
    assert(PolygonalNumber5.isPentagonalNumber(70))
  }

  test("pentagonalNumbers returns stream of pentagonal numbers") {
    assert(PolygonalNumber5.pentagonalNumbers().head === 1)
    assert(PolygonalNumber5.pentagonalNumbers().tail.head === 5)
    assert(PolygonalNumber5.pentagonalNumbers(2).head === 5)
    assert(PolygonalNumber5.pentagonalNumbers().tail.tail.head === 12)
    assert(PolygonalNumber5.pentagonalNumbers().tail.tail.tail.head === 22)
    assert(PolygonalNumber5.pentagonalNumbers().tail.tail.tail.tail.head === 35)
    assert(PolygonalNumber5.pentagonalNumbers().tail.tail.tail.tail.tail.head === 51)
  }

  test("distanceBetweenPentagonalNumbers returns distance between two pentagonal numbers") {
    assert(PolygonalNumber5.distanceBetweenPentagonalNumbers(1, 5) === 34)
    assert(PolygonalNumber5.distanceBetweenPentagonalNumbers(5, 1) === 34)
    assert(PolygonalNumber5.distanceBetweenPentagonalNumbers(1, 1) === 0)
    assert(PolygonalNumber5.distanceBetweenPentagonalNumbers(8, 6) === 41)
  }

  test("precedingPentagonalNumber and succeedingPentagonalNumber return first Pentagonal numbers around target") {
    assert(PolygonalNumber5.precedingPentagonalNumber(70) === 7)
    assert(PolygonalNumber5.precedingPentagonalNumber(60) === 6)
    assert(PolygonalNumber5.succeedingPentagonalNumber(70) === 7)
    assert(PolygonalNumber5.succeedingPentagonalNumber(80) === 8)
  }

  test("firstRemotePentagonalNumber returns index of first pentagonal number " +
    "whose successor is more than specified distance away") {
    assert(PolygonalNumber5.firstRemotePentagonalNumber(6) === 2)
    assert(PolygonalNumber5.firstRemotePentagonalNumber(13) === 5)
  }

  test("firstPentagonalPairWithProperty returns first pentagonal pair with specified property") {
    assert(PolygonalNumber5.firstPentagonalPairWithProperty(1,
      (n1, n2) => PolygonalNumber5.distanceBetweenPentagonalNumbers(n1, n2) >= 10) === (1, 3))
  }

  test("allPentagonalPairsWithinDistanceWithProperty returns all pentagonal pairs with specified property") {
    assert(PolygonalNumber5.allPentagonalPairsWithinDistanceWithProperty(1, 4, 15,
      (n1, n2) => PolygonalNumber5.pentagonalNumber(n1) + PolygonalNumber5.pentagonalNumber(n2) >= 15).
      sortWith(_._1 < _._1) === List((2, 3), (3, 4)))
    assert(PolygonalNumber5.allPentagonalPairsWithinDistanceWithProperty(3, 6, 25,
      (n1, n2) => PolygonalNumber5.pentagonalNumber(n1) + PolygonalNumber5.pentagonalNumber(n2) >= 47).
      sortWith(_._1 < _._1) === List((3, 5), (4, 5), (5, 6)))
  }

  test("pentagonalNumber and getPentagonalNumberIndex should match") {
    val mismatches = for {
      i <- 1 to 10000000
      index = PolygonalNumber5.getPentagonalNumberIndex(PolygonalNumber5.pentagonalNumber(i)).getOrElse(0)
      if i.toLong != index
    } yield (i, index)
    assert(mismatches.isEmpty)
  }
}
