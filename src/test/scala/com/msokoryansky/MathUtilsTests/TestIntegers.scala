package com.msokoryansky.MathUtilsTests

import com.msokoryansky.MathUtils.Integers
import org.scalatest.FunSuite

class TestIntegers extends FunSuite {
  test("ints returns stream of ints") {
    val ints = Integers.ints(0)
    assert(ints.head === 0)
    assert(ints.tail.head === 1)
    assert(ints.tail.tail.head === 2)
    assert(ints.tail.tail.tail.head === 3)
    assert(ints.tail.tail.tail.tail.head === 4)
    assert(ints.tail.tail.tail.tail.tail.head === 5)
    assert(ints.tail.tail.tail.tail.tail.tail.head === 6)
    assert(ints.tail.tail.tail.tail.tail.tail.tail.head === 7)
  }

  test("intsSum returns sum of stream of ints") {
    val ints = Integers.ints(0)
    assert(Integers.intsSum(1, 6, _ => true) === 15)
    assert(Integers.intsSum(1, 8, _ % 2 == 0) === 12)
    assert(Integers.intsSum(4, 11, _ % 3 == 1) === 21)
  }

  test("fibs returns stream of Fibonacci numbers") {
    val fibs = Integers.fibs(0, 1)
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
    assert(Integers.fibsSum(0, 1, 10, _ => true) === 20)
    assert(Integers.fibsSum(0, 1, 10, _ % 2 == 0) === 10)
    assert(Integers.fibsSum(0, 1, 15, _ % 2 == 1) === 23)
  }

  test("primeFactors should factor numbers") {
    assert(Integers.primeFactors(1, Integers.primes(Integers.ints(2)), Nil).sortWith(_ < _) === List())
    assert(Integers.primeFactors(2, Integers.primes(Integers.ints(2)), Nil).sortWith(_ < _) === List(2))
    assert(Integers.primeFactors(24, Integers.primes(Integers.ints(2)), Nil).sortWith(_ < _) === List(2, 2, 2, 3))
    assert(Integers.primeFactors(37, Integers.primes(Integers.ints(2)), Nil).sortWith(_ < _) === List(37))
    assert(Integers.primeFactors(385, Integers.primes(Integers.ints(2)), Nil).sortWith(_ < _) === List(5, 7, 11))
  }

  test("isNumberPalindrome should detect palindromes") {
    assert(Integers.isNumberPalindrome(0))
    assert(Integers.isNumberPalindrome(7))
    assert(Integers.isNumberPalindrome(1221))
    assert(Integers.isNumberPalindrome(12344321))
    assert(!Integers.isNumberPalindrome(253))
    assert(!Integers.isNumberPalindrome(1234474321))
  }

  test("intsDesc produces stream of descending integers") {
    assert(Integers.intsDesc(12, 10).toList === List(12, 11, 10))
    assert(Integers.intsDesc(2, 10).toList === List())
  }

  test("largestProductPalindrome produces largest palindrome that's product of two numbers in integer range") {
    assert(Integers.largestProductPalindrome(10, 1) === 9)
    assert(Integers.largestProductPalindrome(12, 11) === 121)
    assert(Integers.largestProductPalindrome(12, 0) === 121)
    assert(Integers.largestProductPalindrome(4, 0) === 9)
  }

  test("dropFirstMatch drops first occurrence of element from sequence") {
    assert(Integers.dropFirstMatch(List("blah", "bleh", "blah"), "blah") === List("bleh", "blah"))
    assert(Integers.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 6) === Array(7, 8, 7, 6, 0, 1, 6, 8))
    assert(Integers.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 16) === Array(6, 7, 8, 7, 6, 0, 1, 6, 8))
    assert(Integers.dropFirstMatch(Array[Int](), 6) === Array[Int]())
    assert(Integers.dropFirstMatch(Array(10), 10) === Array())
    assert(Integers.dropFirstMatch(Array(6, 7, 8, 7, 6, 0, 1, 6, 8), 0) === Array(6, 7, 8, 7, 6, 1, 6, 8))
  }

  test("union2 drops creates union of two sequences with possible repeating elements") {
    assert(Integers.union2(List("blah", "bleh", "blah"), List("blah")).sortWith(_ > _) === List("blah", "bleh", "blah").sortWith(_ > _))
    assert(Integers.union2(Array(6, 6, 8), Array(6, 7, 6, 6, 9)).sortWith(_ > _) === Array(6, 6, 8, 6, 7, 9).sortWith(_ > _))
    assert(Integers.union2(Array(6, 6, 8), Array[Int]()).sortWith(_ > _) === Array(6, 6, 8).sortWith(_ > _))
    assert(Integers.union2(Array[BigInt](), Array[BigInt]()).sortWith(_ > _) === Array[BigInt]().sortWith(_ > _))
  }

  test("primeFactorsOfRange returns sequence of prime factors that jointly factor any number in specified range ") {
    assert(Integers.primeFactorsOfRange(1, 4).sortWith(_ > _) === Seq(2, 2, 3).sortWith(_ > _))
    assert(Integers.primeFactorsOfRange(2, 8).sortWith(_ > _) === Seq(2, 3, 2, 5, 7, 2).sortWith(_ > _))
  }

  test("ints creates integer sequence starting with specified number") {
    assert(Integers.ints(2).head === 2)
    assert(Integers.ints(3).tail.head === 4)
    assert(Integers.ints(3).tail.tail.tail.head === 6)
  }

  test("isPrime tests if number is prime") {
    assert(Integers.isPrime(2) === true)
    assert(Integers.isPrime(-1) === false)
    assert(Integers.isPrime(1) === false)
    assert(Integers.isPrime(12) === false)
    assert(Integers.isPrime(29) === true)
  }

  test("primeNumber returns nth number in primes sequence") {
    assert(Integers.primeNumber(-1) === 2)
    assert(Integers.primeNumber(0) === 2)
    assert(Integers.primeNumber(1) === 2)
    assert(Integers.primeNumber(2) === 3)
    assert(Integers.primeNumber(5) === 11)
  }
}
