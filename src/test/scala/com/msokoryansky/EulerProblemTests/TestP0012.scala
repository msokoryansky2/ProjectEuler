package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0012 extends FunSuite {
  val p = new P0012

  test("divisors returns list of unique divisors of a number") {
    intercept[Exception] {
      p.divisors(-1)
    }
    intercept[Exception] {
      p.divisors(0)
    }
    assert(p.divisors(1).toList.sortWith(_ < _) === List(1))
    assert(p.divisors(12).toList.sortWith(_ < _) === List(1, 2, 3, 4, 6, 12))
    assert(p.divisors(27).toList.sortWith(_ < _) === List(1, 3, 9, 27))
    assert(p.divisors(31).toList.sortWith(_ < _) === List(1, 31))
  }

  test("triangleNumbers returns stream of Triangle numbers") {
    assert(p.triangleNumbers(0, 0).head === 1)
    assert(p.triangleNumbers(0, 0).tail.head === 3)
    assert(p.triangleNumbers(0, 0).tail.tail.head === 6)
    assert(p.triangleNumbers(0, 0).tail.tail.tail.head === 10)
    assert(p.triangleNumbers(0, 0).tail.tail.tail.tail.head === 15)
    assert(p.triangleNumbers(0, 0).tail.tail.tail.tail.tail.head === 21)
    assert(p.triangleNumbers(0, 0).tail.tail.tail.tail.tail.tail.head === 28)
  }
}