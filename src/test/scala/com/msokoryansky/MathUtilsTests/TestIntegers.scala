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

  test("P4.intsDesc produces stream of descending integers") {
    assert(Integers.intsDesc(12, 10).toList === List(12, 11, 10))
    assert(Integers.intsDesc(2, 10).toList === List())
  }

  test("P4.largestProductPalindrome produces largest palindrome that's product of two numbers in integer range") {
    assert(Integers.largestProductPalindrome(10, 1) === 9)
    assert(Integers.largestProductPalindrome(12, 11) === 121)
    assert(Integers.largestProductPalindrome(12, 0) === 121)
    assert(Integers.largestProductPalindrome(4, 0) === 9)
  }
}
