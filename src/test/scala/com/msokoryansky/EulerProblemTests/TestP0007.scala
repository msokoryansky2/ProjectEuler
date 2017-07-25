package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0007 extends FunSuite {
  test("P7.ints creates integer sequence starting with specified number") {
    val p7 = new P0007
    assert(p7.ints(2).head === 2)
    assert(p7.ints(3).tail.head === 4)
    assert(p7.ints(3).tail.tail.tail.head === 6)
  }

  test("P7.isPrime tests if number is prime") {
    val p7 = new P0007
    assert(p7.isPrime(2) === true)
    assert(p7.isPrime(-1) === false)
    assert(p7.isPrime(1) === false)
    assert(p7.isPrime(12) === false)
    assert(p7.isPrime(29) === true)
  }

  test("P7.primeNumber returns nth number in primes sequence") {
    val p7 = new P0007
    assert(p7.primeNumber(-1) === 2)
    assert(p7.primeNumber(0) === 2)
    assert(p7.primeNumber(1) === 2)
    assert(p7.primeNumber(2) === 3)
    assert(p7.primeNumber(5) === 11)
  }
}