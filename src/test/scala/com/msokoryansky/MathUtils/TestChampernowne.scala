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
}
