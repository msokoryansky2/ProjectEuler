package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestFibonacci extends FunSuite {
  test("fibs returns stream of Fibonacci numbers") {
    val fibs = Fibonacci.fibs(0, 1)
    assert(fibs.head === 0)
    assert(fibs.tail.head === 1)
    assert(fibs.tail.tail.head === 1)
    assert(fibs.tail.tail.tail.head === 2)
    assert(fibs.tail.tail.tail.tail.head === 3)
    assert(fibs.tail.tail.tail.tail.tail.head === 5)
    assert(fibs.tail.tail.tail.tail.tail.tail.head === 8)
    assert(fibs.tail.tail.tail.tail.tail.tail.tail.head === 13)
  }

  test("fibsSum returns sum of stream of fibs") {
    assert(Fibonacci.fibsSum(0, 1, 10, _ => true) === 20)
    assert(Fibonacci.fibsSum(0, 1, 10, _ % 2 == 0) === 10)
    assert(Fibonacci.fibsSum(0, 1, 15, _ % 2 == 1) === 23)
  }
}
