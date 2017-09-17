package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite
import mike.sokoryansky.MathUtils.IntegerOps.DigitOps

import scala.collection.immutable.HashSet

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

  test("trims returns lists of integers formed by trimming off digits") {
    intercept[Exception] {
      Integer.trimsRight(-1)
    }
    intercept[Exception] {
      Integer.trimsLeft(-1)
    }
    assert(Integer.trimsRight(0) === List(0))
    assert(Integer.trimsRight(1) === List(1))
    assert(Integer.trimsRight(10) === List(1, 10))
    assert(Integer.trimsRight(1000) === List(1, 10, 100, 1000))
    assert(Integer.trimsRight(13) === List(1, 13))
    assert(Integer.trimsRight(123) === List(1, 12, 123))
    assert(Integer.trimsRight(1234) === List(1, 12, 123, 1234))
    assert(Integer.trimsRight(1020034) === List(1, 10, 102, 1020, 10200, 102003, 1020034))
    assert(Integer.trimsLeft(0) === List(0))
    assert(Integer.trimsLeft(1) === List(1))
    assert(Integer.trimsLeft(10) === List(0, 10))
    assert(Integer.trimsLeft(100) === List(0, 100))
    assert(Integer.trimsLeft(1000) === List(0, 1000))
    assert(Integer.trimsLeft(13) === List(3, 13))
    assert(Integer.trimsLeft(123) === List(23, 3, 123))
    assert(Integer.trimsLeft(1234) === List(234, 34, 4, 1234))
    assert(Integer.trimsLeft(1020034) === List(20034, 34, 4, 1020034))
  }

  test("subsetsWithFixedDigits returns all possible non-empty fixed-digit permutations") {
    val set0 = (1 to 9).map(d => Map(0 -> d)).toSet
    val set1 = (0 to 9).map(d => Map(1 -> d)).toSet
    val set2 = (0 to 9).map(d => Map(2 -> d)).toSet
    val set01 = (1 to 9).flatMap(d0 => (0 to 9).map(d1 => Map(0 -> d0, 1 -> d1))).toSet
    val set02 = (1 to 9).flatMap(d0 => (0 to 9).map(d2 => Map(0 -> d0, 2 -> d2))).toSet
    val set12 = (0 to 9).flatMap(d1 => (0 to 9).map(d2 => Map(1 -> d1, 2 -> d2))).toSet
    val set012 = (1 to 9).flatMap(d0 => (0 to 9).flatMap(d1 => (0 to 9).map(d2 => Map(0 -> d0, 1 -> d1, 2 -> d2)))).toSet

    assert(Integer.subsetsWithFixedDigits(1) === set0)
    assert(Integer.subsetsWithFixedDigits(2) === set0 ++ set1 ++ set01)
    assert(Integer.subsetsWithFixedDigits(3) === set0 ++ set1 ++ set2 ++ set01 ++ set02 ++ set12 ++ set012)
  }

  test("decatenate finds which parts a number can be split into") {
    assert(Integer.decatenate(1234567890, IndexedSeq[Long](123, 456, 7890, 123456, 12, 34567890)).toSet ===
      Seq((123456, 7890), (12, 34567890)).toSet)
    assert(Integer.decatenate(95035, IndexedSeq[Long](35, 95, 950, 350)).toSet
      === Seq((950, 35)).toSet)
  }

  test("isPow checks if a number is a specified power") {
    assert(Integer.isPow(100, 2))
    assert(Integer.isPow(27, 3))
    assert(Integer.isPow(256, 8))
    assert(!Integer.isPow(1023, 10))
    assert(Integer.isPow(1024, 10))
    assert(Integer.isPow(41063625, 3))
    assert(Integer.isPow(56623104, 3))
    assert(!Integer.isPow(41063626, 3))
    assert(!Integer.isPow(56623103, 3))
  }

  test("sumDigits returns sum of digits") {
    assert(0.sumDigits === 0)
    assert(1.sumDigits === 1)
    assert(100000.sumDigits === 1)
    assert(123456789.sumDigits === 45)
    assert(1000000000000000L.sumDigits === 1)
    assert(BigInt("123456789012345678901234567890").sumDigits === 135)
    assert(-100000.sumDigits === -1)
    assert(-123456789.sumDigits === -45)
    assert(-1000000000000000L.sumDigits === -1)
    assert(BigInt("-123456789012345678901234567890").sumDigits === -135)
  }

  test("isPermutation checks if two numbers are permutations of digits of one another") {
    assert(Integer.isPermutation(123, 321))
    assert(Integer.isPermutation(12300, 30021))
    assert(Integer.isPermutation(33445566777888L, 87878765433456L))
    assert(!Integer.isPermutation(334455660777888L, 87878765433456L))
  }

  test("gcd computes greatest common divisor") {
    intercept[Exception] {
      Integer.gcd(0, 1)
    }
    assert(Integer.gcd(1, 1) === 1)
    assert(Integer.gcd(1, 2) === 1)
    assert(Integer.gcd(2, 1) === 1)
    assert(Integer.gcd(1, 17) === 1)
    assert(Integer.gcd(17, 1) === 1)
    assert(Integer.gcd(23, 17) === 1)
    assert(Integer.gcd(17, 23) === 1)
    assert(Integer.gcd(66, 33) === 33)
    assert(Integer.gcd(33, 66) === 33)
    assert(Integer.gcd(48, 72) === 24)
    assert(Integer.gcd(72, 48) === 24)
    assert(Integer.gcd(600, 720) === 120)
    assert(Integer.gcd(720, 600) === 120)
  }

  test("sumDigitsFactorial returns sum of factorials of a number's digits") {
    assert(Integer.sumDigitsFactorial(0) === 1)
    assert(Integer.sumDigitsFactorial(100) === 3)
    assert(Integer.sumDigitsFactorial(145) === 145)
    assert(Integer.sumDigitsFactorial(871) === 45361)
    assert(Integer.sumDigitsFactorial(872) === 45362)
  }

  test("sumDigitsFactorialChainLength returns the length of chain of sums of digits' factorials before first repeat") {
    /*
    145 (→ 145)
    169 → 363601 → 1454 → (169)
    871 → 45361 → (871)
    872 → 45362 → (872)
    69 → 363600 → 1454 → 169 → 363601 (→ 1454)
    78 → 45360 → 871 → 45361 (→ 871)
    540 → 145 (→ 145)
     */
    assert(Integer.sumDigitsFactorialChainLength(145) === 1)
    assert(Integer.sumDigitsFactorialChainLength(169) === 3)
    assert(Integer.sumDigitsFactorialChainLength(871) === 2)
    assert(Integer.sumDigitsFactorialChainLength(872) === 2)
    assert(Integer.sumDigitsFactorialChainLength(69) === 5)
    assert(Integer.sumDigitsFactorialChainLength(78) === 4)
    assert(Integer.sumDigitsFactorialChainLength(540) === 2)
  }

  test("sumDigitsFactorialChainLength1ToN computes sumDigitsFactorialChainLength for all integers from 1 to N") {
    val lengths = Integer.sumDigitsFactorialChainLength1ToN(1000)
    assert(lengths.size === 1000)
    assert(lengths(145) === 1)
    assert(lengths(169) === 3)
    assert(lengths(871) === 2)
    assert(lengths(872) === 2)
    assert(lengths(69) === 5)
    assert(lengths(78) === 4)
    assert(lengths(540) === 2)
    (1 to 1000).map(i => {
      assert(lengths.contains(i))
      assert(lengths(i) === Integer.sumDigitsFactorialChainLength(i))
    })
  }

  test("sums returns all unique ways to break an integer into sums of smaller integers") {
    assert(Integer.sums(2) === Set(Map(1 -> 2)))
    assert(Integer.sums(3) === Set(Map(1 -> 3), Map(2 -> 1, 1 -> 1)))
    assert(Integer.sums(4) === Set(Map(1 -> 4), Map(2 -> 1, 1 -> 2), Map(2 -> 2), Map(3 -> 1, 1 -> 1)))
    assert(Integer.sums(5) === Set(Map(1 -> 5), Map(2 -> 1, 1 -> 3), Map(2 -> 2, 1 -> 1),
                                    Map(3 -> 1, 1 -> 2), Map(3 -> 1, 2 -> 1), Map(4 -> 1, 1 -> 1)))
    assert(Integer.sums(6) === Set(Map(1 -> 6), Map(2 -> 1, 1 -> 4), Map(2 -> 2, 1 -> 2), Map(2 -> 3),
                                    Map(3 -> 1, 1 -> 3), Map(3 -> 1, 2 -> 1, 1 -> 1), Map(3 -> 2),
                                    Map(4 -> 1, 1 -> 2), Map(4 -> 1, 2 -> 1), Map(5 -> 1, 1 -> 1)))
  }
}