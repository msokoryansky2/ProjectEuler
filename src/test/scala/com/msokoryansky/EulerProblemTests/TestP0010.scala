package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0010 extends FunSuite {
  test("isPrime checks if number is prime") {
    val p = new P0010
    assert(p.isPrime(-7) === false)
    assert(p.isPrime(-2) === false)
    assert(p.isPrime(-1) === false)
    assert(p.isPrime(0) === false)
    assert(p.isPrime(1) === false)
    assert(p.isPrime(2) === true)
    assert(p.isPrime(3) === true)
    assert(p.isPrime(4) === false)
    assert(p.isPrime(13) === true)
    assert(p.isPrime(16) === false)
    assert(p.isPrime(21) === false)
    assert(p.isPrime(37) === true)
  }

  test("nextPrime returns next smallest prime >= specified argument") {
    val p = new P0010
    assert(p.nextPrime(-7) === 2)
    assert(p.nextPrime(-2) === 2)
    assert(p.nextPrime(-1) === 2)
    assert(p.nextPrime(0) === 2)
    assert(p.nextPrime(1) === 2)
    assert(p.nextPrime(2) === 2)
    assert(p.nextPrime(3) === 3)
    assert(p.nextPrime(4) === 5)
    assert(p.nextPrime(13) === 13)
    assert(p.nextPrime(16) === 17)
    assert(p.nextPrime(21) === 23)
    assert(p.nextPrime(37) === 37)
  }

  test("primeNumberSum returns sum of all primes < specified argument") {
    val p = new P0010
    assert(p.primeNumberSum(-7) === 0)
    assert(p.primeNumberSum(-2) === 0)
    assert(p.primeNumberSum(-1) === 0)
    assert(p.primeNumberSum(0) === 0)
    assert(p.primeNumberSum(1) === 0)
    assert(p.primeNumberSum(2) === 0)
    assert(p.primeNumberSum(3) === 2)
    assert(p.primeNumberSum(4) === 5)
    assert(p.primeNumberSum(13) === 28)
    assert(p.primeNumberSum(16) === 41)
    assert(p.primeNumberSum(21) === 77)
  }
}