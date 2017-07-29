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
    assert(Integers.union2(List("blah", "bleh", "blah"), List("blah")).sortWith(_ > _)
      === List("blah", "bleh", "blah").sortWith(_ > _))
    assert(Integers.union2(Array(6, 6, 8), Array(6, 7, 6, 6, 9)).sortWith(_ > _)
      === Array(6, 6, 8, 6, 7, 9).sortWith(_ > _))
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

  test("greatestProduct returns largest product of consecutive digits in a string representation of a number") {
    assert(Integers.greatestProduct("123456", 3) === 120)
    assert(Integers.greatestProduct("123456", 7) === 0)
    assert(Integers.greatestProduct("123", 3) === 6)
    assert(Integers.greatestProduct("9999022222", 5) === 32)
    assert(Integers.greatestProduct("949596", 3) === 405)
  }

  test("pythagoreanC returns Option(c) so that c is a pythagorean match to a and b and a + b + c add up to sum") {
    assert(Integers.pythagoreanC(1, 2, 3) === None)
    assert(Integers.pythagoreanC(3, 4, 12) === Some(5))
  }

  test("pythagoreanTripletBySum returns Option(a, b, c) so that they are a pythagorean triple adding up to sum") {
    assert(Integers.pythagoreanTripletBySum(7) === None)
    assert(Integers.pythagoreanTripletBySum(12) === Some((3, 4, 5))
      || Integers.pythagoreanTripletBySum(12) === Some(4, 3, 5))
    assert(Integers.pythagoreanProduct(Integers.pythagoreanTripletBySum(7)) === "None")
    assert(Integers.pythagoreanProduct(Integers.pythagoreanTripletBySum(12)) === "60")
  }

  test("isPrime checks if number is prime") {
    assert(Integers.isPrime(-7) === false)
    assert(Integers.isPrime(-2) === false)
    assert(Integers.isPrime(-1) === false)
    assert(Integers.isPrime(0) === false)
    assert(Integers.isPrime(1) === false)
    assert(Integers.isPrime(2) === true)
    assert(Integers.isPrime(3) === true)
    assert(Integers.isPrime(4) === false)
    assert(Integers.isPrime(13) === true)
    assert(Integers.isPrime(16) === false)
    assert(Integers.isPrime(21) === false)
    assert(Integers.isPrime(37) === true)
  }

  test("nextPrime returns next smallest prime >= specified argument") {
    assert(Integers.nextPrime(-7) === 2)
    assert(Integers.nextPrime(-2) === 2)
    assert(Integers.nextPrime(-1) === 2)
    assert(Integers.nextPrime(0) === 2)
    assert(Integers.nextPrime(1) === 2)
    assert(Integers.nextPrime(2) === 2)
    assert(Integers.nextPrime(3) === 3)
    assert(Integers.nextPrime(4) === 5)
    assert(Integers.nextPrime(13) === 13)
    assert(Integers.nextPrime(16) === 17)
    assert(Integers.nextPrime(21) === 23)
    assert(Integers.nextPrime(37) === 37)
  }

  test("primeNumberSum returns sum of all primes < specified argument") {
    assert(Integers.primeNumberSum(-7) === 0)
    assert(Integers.primeNumberSum(-2) === 0)
    assert(Integers.primeNumberSum(-1) === 0)
    assert(Integers.primeNumberSum(0) === 0)
    assert(Integers.primeNumberSum(1) === 0)
    assert(Integers.primeNumberSum(2) === 0)
    assert(Integers.primeNumberSum(3) === 2)
    assert(Integers.primeNumberSum(4) === 5)
    assert(Integers.primeNumberSum(13) === 28)
    assert(Integers.primeNumberSum(16) === 41)
    assert(Integers.primeNumberSum(21) === 77)
  }

  test("divisors returns list of unique divisors of a number") {
    intercept[Exception] {
      Integers.divisors(-1)
    }
    intercept[Exception] {
      Integers.divisors(0)
    }
    assert(Integers.divisors(1).toList.sortWith(_ < _) === List(1))
    assert(Integers.divisors(12).toList.sortWith(_ < _) === List(1, 2, 3, 4, 6, 12))
    assert(Integers.divisors(27).toList.sortWith(_ < _) === List(1, 3, 9, 27))
    assert(Integers.divisors(31).toList.sortWith(_ < _) === List(1, 31))
  }

  test("triangleNumbers returns stream of Triangle numbers") {
    assert(Integers.triangleNumbers(0, 0).head === 1)
    assert(Integers.triangleNumbers(0, 0).tail.head === 3)
    assert(Integers.triangleNumbers(0, 0).tail.tail.head === 6)
    assert(Integers.triangleNumbers(0, 0).tail.tail.tail.head === 10)
    assert(Integers.triangleNumbers(0, 0).tail.tail.tail.tail.head === 15)
    assert(Integers.triangleNumbers(0, 0).tail.tail.tail.tail.tail.head === 21)
    assert(Integers.triangleNumbers(0, 0).tail.tail.tail.tail.tail.tail.head === 28)
  }
}
