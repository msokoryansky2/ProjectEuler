package com.msokoryansky.MathUtils

import org.scalatest.FunSuite
import  com.msokoryansky.MathUtils.IntegerOps.DigitOps

class TestInteger extends FunSuite {
  test("ints returns stream of ints") {
    val ints = Integer.ints(0)
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
    val ints = Integer.ints(0)
    assert(Integer.intsSum(1, 6, _ => true) === 15)
    assert(Integer.intsSum(1, 8, _ % 2 == 0) === 12)
    assert(Integer.intsSum(4, 11, _ % 3 == 1) === 21)
  }

  test("intsDesc produces stream of descending integers") {
    assert(Integer.intsDesc(12, 10).toList === List(12, 11, 10))
    assert(Integer.intsDesc(2, 10).toList === List())
  }

  test("ints creates integer sequence starting with specified number") {
    assert(Integer.ints(2).head === 2)
    assert(Integer.ints(3).tail.head === 4)
    assert(Integer.ints(3).tail.tail.tail.head === 6)
  }

  test("greatestProduct returns largest product of consecutive digits in a string representation of a number") {
    assert(Integer.greatestProduct("123456", 3) === 120)
    assert(Integer.greatestProduct("123456", 7) === 0)
    assert(Integer.greatestProduct("123", 3) === 6)
    assert(Integer.greatestProduct("9999022222", 5) === 32)
    assert(Integer.greatestProduct("949596", 3) === 405)
  }

  test("divisors returns list of unique divisors of a number") {
    intercept[Exception] {
      Integer.divisors(-1)
    }
    intercept[Exception] {
      Integer.divisors(0)
    }
    assert(Integer.divisors(1).toList.sortWith(_ < _) === List(1))
    assert(Integer.divisors(12).toList.sortWith(_ < _) === List(1, 2, 3, 4, 6, 12))
    assert(Integer.divisors(27).toList.sortWith(_ < _) === List(1, 3, 9, 27))
    assert(Integer.divisors(31).toList.sortWith(_ < _) === List(1, 31))
  }

  test("isSumOf2 checks if specified number is a sum of two elements in specified set") {
    assert(Integer.isSumOf2Elements(12, Set(4, 5, 6, 7)))
    assert(Integer.isSumOf2Elements(12, Set(4, 5, 6, 9)))
    assert(!Integer.isSumOf2Elements(12, Set(4, 5, 10, 9)))
    assert(Integer.isSumOf2Elements(55, Set(10, 30, 20, 40, 5, 25)))
    assert(!Integer.isSumOf2Elements(55, Set(10, 30, 20, 40, 50, 60)))
  }

  test("IntegerOps.DigitsOps.mapDigits applies specified function to every digit of a number and returns the resulting list") {
    assert(123.mapDigits(i => i).mkString === "123")
    assert(456.mapDigits(_ * 2) === List(8, 10, 12))
    assert(123.mapDigits(_.pow(2)) === List(1, 4, 9))
  }

  test("isSumDigitFactorials checks if a number is equal to sum of its digits' factorials") {
    assert(!Integer.isSumDigitFactorials(123))
    assert(Integer.isSumDigitFactorials(145))
  }

  test("circulars return all numbers created by rotating digits of a number") {
    assert(Integer.circulars(7) == List(7))
    assert(Integer.circulars(912) == List(912, 291, 129))
    assert(Integer.circulars(902) == List(902, 290, 29))
  }

  test("base2 converts integer to base 2") {
    intercept[Exception] {
      Integer.base2(-1)
    }
    assert(Integer.base2(0) === "0")
    assert(Integer.base2(1) === "1")
    assert(Integer.base2(2) === "10")
    assert(Integer.base2(3) === "11")
    assert(Integer.base2(4) === "100")
    assert(Integer.base2(5) === "101")
    assert(Integer.base2(6) === "110")
    assert(Integer.base2(7) === "111")
    assert(Integer.base2(8) === "1000")
    assert(Integer.base2(15) === "1111")
    assert(Integer.base2(16) === "10000")
    assert(Integer.base2(17) === "10001")
    assert(Integer.base2(31) === "11111")
    assert(Integer.base2(32) === "100000")
    assert(Integer.base2(33) === "100001")
  }
}