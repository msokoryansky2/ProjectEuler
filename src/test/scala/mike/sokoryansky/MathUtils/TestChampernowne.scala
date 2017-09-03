package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestChampernowne extends FunSuite {
  test("startOfNDigitNumbers computes starting place after the decimal point of N-digit numbers in Champernowne") {
    intercept[Exception] {
      Champernowne.startOfNDigitNumbers(0)
    }
    assert(Champernowne.startOfNDigitNumbers(1) === 1)
    assert(Champernowne.startOfNDigitNumbers(2) === 10)
    assert(Champernowne.startOfNDigitNumbers(3) === 190)
    assert(Champernowne.startOfNDigitNumbers(4) === 2890)
    assert(Champernowne.startOfNDigitNumbers(5) === 38890)
  }

  test("getNDigitNumber returns the digit range (e.g. 2-digit, 5-digit, etc) for given Champernowne digit number") {
    intercept[Exception] {
      Champernowne.getNDigitNumber(0)
    }
    assert(Champernowne.getNDigitNumber(1) === 1)
    assert(Champernowne.getNDigitNumber(5) === 1)
    assert(Champernowne.getNDigitNumber(9) === 1)
    assert(Champernowne.getNDigitNumber(10) === 2)
    assert(Champernowne.getNDigitNumber(100) === 2)
    assert(Champernowne.getNDigitNumber(189) === 2)
    assert(Champernowne.getNDigitNumber(190) === 3)
    assert(Champernowne.getNDigitNumber(1000) === 3)
    assert(Champernowne.getNDigitNumber(2889) === 3)
    assert(Champernowne.getNDigitNumber(2890) === 4)
    assert(Champernowne.getNDigitNumber(10000) === 4)
    assert(Champernowne.getNDigitNumber(38889) === 4)
  }

  test("digit returns nth digit within Champernowne decimal sequence") {
    intercept[Exception] {
      Champernowne.digit(0)
    }
    assert(Champernowne.digit(1) === 1)
    assert(Champernowne.digit(5) === 5)
    assert(Champernowne.digit(9) === 9)
    assert(Champernowne.digit(10) === 1)
    assert(Champernowne.digit(11) === 0)
    assert(Champernowne.digit(12) === 1)
    assert(Champernowne.digit(13) === 1)
    assert(Champernowne.digit(14) === 1)
    assert(Champernowne.digit(15) === 2)
    assert(Champernowne.digit(16) === 1)
    assert(Champernowne.digit(17) === 3)
    assert(Champernowne.digit(188) === 9)
    assert(Champernowne.digit(189) === 9)
    assert(Champernowne.digit(190) === 1)
    assert(Champernowne.digit(191) === 0)
    assert(Champernowne.digit(192) === 0)
    assert(Champernowne.digit(193) === 1)
    assert(Champernowne.digit(194) === 0)
    assert(Champernowne.digit(195) === 1)
    assert(Champernowne.digit(2884) === 9)
    assert(Champernowne.digit(2885) === 9)
    assert(Champernowne.digit(2886) === 8)
    assert(Champernowne.digit(2887) === 9)
    assert(Champernowne.digit(2888) === 9)
    assert(Champernowne.digit(2889) === 9)
    assert(Champernowne.digit(2890) === 1)
    assert(Champernowne.digit(2891) === 0)
    assert(Champernowne.digit(2892) === 0)
    assert(Champernowne.digit(2893) === 0)
    assert(Champernowne.digit(2894) === 1)
    assert(Champernowne.digit(2895) === 0)
    assert(Champernowne.digit(2896) === 0)
    assert(Champernowne.digit(2897) === 1)
    assert(Champernowne.digit(2890) === 1)
    assert(Champernowne.digit(38882) === 9)
    assert(Champernowne.digit(38883) === 9)
    assert(Champernowne.digit(38884) === 9)
    assert(Champernowne.digit(38885) === 8)
    assert(Champernowne.digit(38886) === 9)
    assert(Champernowne.digit(38887) === 9)
    assert(Champernowne.digit(38888) === 9)
    assert(Champernowne.digit(38889) === 9)
    assert(Champernowne.digit(38890) === 1)
    assert(Champernowne.digit(38891) === 0)
    assert(Champernowne.digit(38892) === 0)
    assert(Champernowne.digit(38893) === 0)
    assert(Champernowne.digit(38894) === 0)
    assert(Champernowne.digit(38895) === 1)
    assert(Champernowne.digit(38896) === 0)
    assert(Champernowne.digit(38897) === 0)
    assert(Champernowne.digit(38898) === 0)
    assert(Champernowne.digit(38899) === 1)
  }
}
