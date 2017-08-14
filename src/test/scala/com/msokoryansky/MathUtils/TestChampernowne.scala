package com.msokoryansky.MathUtils

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
}
