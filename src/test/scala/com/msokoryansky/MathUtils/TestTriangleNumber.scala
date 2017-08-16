package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestTriangleNumber extends FunSuite {
  test("triangleNumber returns triangle number specified by the index") {
    intercept[Exception] {
      TriangleNumber.triangleNumber(0)
    }
    assert(TriangleNumber.triangleNumber(1) === 1)
    assert(TriangleNumber.triangleNumber(2) === 3)
    assert(TriangleNumber.triangleNumber(3) === 6)
    assert(TriangleNumber.triangleNumber(4) === 10)
    assert(TriangleNumber.triangleNumber(5) === 15)
    assert(TriangleNumber.triangleNumber(6) === 21)
    assert(TriangleNumber.triangleNumber(7) === 28)
    assert(TriangleNumber.triangleNumber(8) === 36)
    assert(TriangleNumber.triangleNumber(9) === 45)
    assert(TriangleNumber.triangleNumber(10) === 55)
  }

  test("getTriangleNumberIndex returns Some of specified number's in triangle number series or None") {
    assert(TriangleNumber.getTriangleNumberIndex(0) === None)
    assert(TriangleNumber.getTriangleNumberIndex(1) === Some(1))
    assert(TriangleNumber.getTriangleNumberIndex(2) === None)
    assert(TriangleNumber.getTriangleNumberIndex(3) === Some(2))
    assert(TriangleNumber.getTriangleNumberIndex(4) === None)
    assert(TriangleNumber.getTriangleNumberIndex(10) === Some(4))
    assert(TriangleNumber.getTriangleNumberIndex(50) === None)
    assert(TriangleNumber.getTriangleNumberIndex(55) === Some(10))
  }

  test("isTriangleNumber returns if number is a triangle one") {
    assert(!TriangleNumber.isTriangleNumber(0))
    assert(TriangleNumber.isTriangleNumber(1))
    assert(!TriangleNumber.isTriangleNumber(2))
    assert(TriangleNumber.isTriangleNumber(3))
    assert(!TriangleNumber.isTriangleNumber(4))
    assert(TriangleNumber.isTriangleNumber(10))
    assert(!TriangleNumber.isTriangleNumber(50))
    assert(TriangleNumber.isTriangleNumber(55))
  }

  test("triangleNumber and getTriangleNumberIndex should match") {
    val mismatches = for {
      i <- 1 to 10000000
      index = TriangleNumber.getTriangleNumberIndex(TriangleNumber.triangleNumber(i)).getOrElse(0)
      if i != index
    } yield (i, index)
    assert(mismatches.isEmpty)
  }
}
