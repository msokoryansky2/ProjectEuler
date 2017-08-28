package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPolygonalNumber3 extends FunSuite {
  test("triangleNumber returns triangle number specified by the index") {
    intercept[Exception] {
      PolygonalNumber3.triangleNumber(0)
    }
    assert(PolygonalNumber3.triangleNumber(1) === 1)
    assert(PolygonalNumber3.triangleNumber(2) === 3)
    assert(PolygonalNumber3.triangleNumber(3) === 6)
    assert(PolygonalNumber3.triangleNumber(4) === 10)
    assert(PolygonalNumber3.triangleNumber(5) === 15)
    assert(PolygonalNumber3.triangleNumber(6) === 21)
    assert(PolygonalNumber3.triangleNumber(7) === 28)
    assert(PolygonalNumber3.triangleNumber(8) === 36)
    assert(PolygonalNumber3.triangleNumber(9) === 45)
    assert(PolygonalNumber3.triangleNumber(10) === 55)
  }

  test("getTriangleNumberIndex returns Some of specified number's in triangle number series or None") {
    assert(PolygonalNumber3.getTriangleNumberIndex(0) === None)
    assert(PolygonalNumber3.getTriangleNumberIndex(1) === Some(1))
    assert(PolygonalNumber3.getTriangleNumberIndex(2) === None)
    assert(PolygonalNumber3.getTriangleNumberIndex(3) === Some(2))
    assert(PolygonalNumber3.getTriangleNumberIndex(4) === None)
    assert(PolygonalNumber3.getTriangleNumberIndex(10) === Some(4))
    assert(PolygonalNumber3.getTriangleNumberIndex(50) === None)
    assert(PolygonalNumber3.getTriangleNumberIndex(55) === Some(10))
  }

  test("isTriangleNumber returns if number is a triangle one") {
    assert(!PolygonalNumber3.isTriangleNumber(0))
    assert(PolygonalNumber3.isTriangleNumber(1))
    assert(!PolygonalNumber3.isTriangleNumber(2))
    assert(PolygonalNumber3.isTriangleNumber(3))
    assert(!PolygonalNumber3.isTriangleNumber(4))
    assert(PolygonalNumber3.isTriangleNumber(10))
    assert(!PolygonalNumber3.isTriangleNumber(50))
    assert(PolygonalNumber3.isTriangleNumber(55))
  }

  test("triangleNumber and getTriangleNumberIndex should match") {
    val mismatches = for {
      i <- 1 to 10000000
      index = PolygonalNumber3.getTriangleNumberIndex(PolygonalNumber3.triangleNumber(i)).getOrElse(0)
      if i.toLong != index
    } yield (i, index)
    assert(mismatches.isEmpty)
  }
}
