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

  test("getTriangleNumberNumber returns Some of specified number's in triangle number series or None") {
    assert(TriangleNumber.getTriangleNumberNumber(0) === None)
    assert(TriangleNumber.getTriangleNumberNumber(1) === Some(1))
    assert(TriangleNumber.getTriangleNumberNumber(2) === None)
    assert(TriangleNumber.getTriangleNumberNumber(3) === Some(2))
    assert(TriangleNumber.getTriangleNumberNumber(4) === None)
    assert(TriangleNumber.getTriangleNumberNumber(10) === Some(4))
    assert(TriangleNumber.getTriangleNumberNumber(50) === None)
    assert(TriangleNumber.getTriangleNumberNumber(55) === Some(10))
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
}
