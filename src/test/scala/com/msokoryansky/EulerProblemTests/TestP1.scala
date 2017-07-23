package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP1 extends FunSuite {
  test("P1.ints returns stream of ints") {
    val p1 = new P1
    val ints = p1.ints(0)
    assert(ints.head === 0)
    assert(ints.tail.head === 1)
    assert(ints.tail.tail.head === 2)
    assert(ints.tail.tail.tail.head === 3)
    assert(ints.tail.tail.tail.tail.head === 4)
    assert(ints.tail.tail.tail.tail.tail.head === 5)
    assert(ints.tail.tail.tail.tail.tail.tail.head === 6)
    assert(ints.tail.tail.tail.tail.tail.tail.tail.head === 7)
  }

  test("P1.intsSum returns sum of stream of ints") {
    val p1 = new P1
    val ints = p1.ints(0)
    assert(p1.intsSum(1, 6, _ => true) === 15)
    assert(p1.intsSum(1, 8, _ % 2 == 0) === 12)
    assert(p1.intsSum(4, 11, _ % 3 == 1) === 21)
  }
}