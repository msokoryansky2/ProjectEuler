package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPrime extends FunSuite {
  test("primeFactors should factor numbers") {
    assert(Prime.primeFactors(1, Prime.primes(Integer.ints(2)), Nil).sortWith(_ < _) === List())
    assert(Prime.primeFactors(2, Prime.primes(Integer.ints(2)), Nil).sortWith(_ < _) === List(2))
    assert(Prime.primeFactors(24, Prime.primes(Integer.ints(2)), Nil).sortWith(_ < _) === List(2, 2, 2, 3))
    assert(Prime.primeFactors(37, Prime.primes(Integer.ints(2)), Nil).sortWith(_ < _) === List(37))
    assert(Prime.primeFactors(385, Prime.primes(Integer.ints(2)), Nil).sortWith(_ < _) === List(5, 7, 11))
  }

  test("primeFactorsOfRange returns sequence of prime factors that jointly factor any number in specified range ") {
    assert(Prime.primeFactorsOfRange(1, 4).sortWith(_ > _) === Seq(2, 2, 3).sortWith(_ > _))
    assert(Prime.primeFactorsOfRange(2, 8).sortWith(_ > _) === Seq(2, 3, 2, 5, 7, 2).sortWith(_ > _))
  }

  test("isPrime tests if number is prime") {
    assert(Prime.isPrime(2) === true)
    assert(Prime.isPrime(-1) === false)
    assert(Prime.isPrime(1) === false)
    assert(Prime.isPrime(12) === false)
    assert(Prime.isPrime(29) === true)
  }

  test("primeNumber returns nth number in primes sequence") {
    assert(Prime.primeNumber(-1) === 2)
    assert(Prime.primeNumber(0) === 2)
    assert(Prime.primeNumber(1) === 2)
    assert(Prime.primeNumber(2) === 3)
    assert(Prime.primeNumber(5) === 11)
  }

  test("isPrime checks if number is prime") {
    assert(Prime.isPrime(-7) === false)
    assert(Prime.isPrime(-2) === false)
    assert(Prime.isPrime(-1) === false)
    assert(Prime.isPrime(0) === false)
    assert(Prime.isPrime(1) === false)
    assert(Prime.isPrime(2) === true)
    assert(Prime.isPrime(3) === true)
    assert(Prime.isPrime(4) === false)
    assert(Prime.isPrime(13) === true)
    assert(Prime.isPrime(16) === false)
    assert(Prime.isPrime(21) === false)
    assert(Prime.isPrime(37) === true)
  }

  test("nextPrime returns next smallest prime >= specified argument") {
    assert(Prime.nextPrime(-7) === 2)
    assert(Prime.nextPrime(-2) === 2)
    assert(Prime.nextPrime(-1) === 2)
    assert(Prime.nextPrime(0) === 2)
    assert(Prime.nextPrime(1) === 2)
    assert(Prime.nextPrime(2) === 2)
    assert(Prime.nextPrime(3) === 3)
    assert(Prime.nextPrime(4) === 5)
    assert(Prime.nextPrime(13) === 13)
    assert(Prime.nextPrime(16) === 17)
    assert(Prime.nextPrime(21) === 23)
    assert(Prime.nextPrime(37) === 37)
  }

  test("primeNumberSum returns sum of all primes < specified argument") {
    assert(Prime.primeNumberSum(-7) === 0)
    assert(Prime.primeNumberSum(-2) === 0)
    assert(Prime.primeNumberSum(-1) === 0)
    assert(Prime.primeNumberSum(0) === 0)
    assert(Prime.primeNumberSum(1) === 0)
    assert(Prime.primeNumberSum(2) === 0)
    assert(Prime.primeNumberSum(3) === 2)
    assert(Prime.primeNumberSum(4) === 5)
    assert(Prime.primeNumberSum(13) === 28)
    assert(Prime.primeNumberSum(16) === 41)
    assert(Prime.primeNumberSum(21) === 77)
  }

}
