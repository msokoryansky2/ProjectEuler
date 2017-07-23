package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP3 extends FunSuite {
  test("P3.primeFactors should factor numbers") {
    val p3 = new P3
    assert(p3.primeFactors(1, p3.primes(p3.ints(2)), Nil).sortWith(_ < _) === List())
    assert(p3.primeFactors(2, p3.primes(p3.ints(2)), Nil).sortWith(_ < _) === List(2))
    assert(p3.primeFactors(24, p3.primes(p3.ints(2)), Nil).sortWith(_ < _) === List(2, 2, 2, 3))
    assert(p3.primeFactors(37, p3.primes(p3.ints(2)), Nil).sortWith(_ < _) === List(37))
    assert(p3.primeFactors(385, p3.primes(p3.ints(2)), Nil).sortWith(_ < _) === List(5, 7, 11))
  }
}