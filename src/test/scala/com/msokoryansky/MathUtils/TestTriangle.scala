package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestTriangle extends FunSuite {
  test("triangleNumbers returns stream of Triangle numbers") {
    assert(Triangle.triangleNumbers(0, 0).head === 1)
    assert(Triangle.triangleNumbers(0, 0).tail.head === 3)
    assert(Triangle.triangleNumbers(0, 0).tail.tail.head === 6)
    assert(Triangle.triangleNumbers(0, 0).tail.tail.tail.head === 10)
    assert(Triangle.triangleNumbers(0, 0).tail.tail.tail.tail.head === 15)
    assert(Triangle.triangleNumbers(0, 0).tail.tail.tail.tail.tail.head === 21)
    assert(Triangle.triangleNumbers(0, 0).tail.tail.tail.tail.tail.tail.head === 28)
  }
}
