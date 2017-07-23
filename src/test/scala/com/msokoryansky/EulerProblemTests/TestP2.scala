package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP2 extends FunSuite {
  test("P2.fibs returns stream of Fibonacci numbers") {
    val p2 = new P2
    val fibs = p2.fibs(0, 1)
    assert(fibs.head === 0)
    assert(fibs.tail.head === 1)
    assert(fibs.tail.tail.head === 1)
    assert(fibs.tail.tail.tail.head === 2)
    assert(fibs.tail.tail.tail.tail.head === 3)
    assert(fibs.tail.tail.tail.tail.tail.head === 5)
    assert(fibs.tail.tail.tail.tail.tail.tail.head === 8)
    assert(fibs.tail.tail.tail.tail.tail.tail.tail.head === 13)
  }

  test("P2.fibsSum returns sum of stream of fibs") {
    val p2 = new P2
    assert(p2.fibsSum(0, 1, 10, _ => true) === 20)
    assert(p2.fibsSum(0, 1, 10, _ % 2 == 0) === 10)
    assert(p2.fibsSum(0, 1, 15, _ % 2 == 1) === 23)
  }
}