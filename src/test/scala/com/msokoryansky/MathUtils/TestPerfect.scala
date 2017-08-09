package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPerfect extends FunSuite {
  test("perfection returns whether a number is Abundant, Perfect, or Deficient") {
    intercept[Exception] {
      Perfect.perfection(-5)
    }
    intercept[Exception] {
      Perfect.perfection(0)
    }
    assert(Perfect.perfection(1) === Perfection.Deficient)
    assert(Perfect.perfection(2) === Perfection.Deficient)
    assert(Perfect.perfection(3) === Perfection.Deficient)
    assert(Perfect.perfection(4) === Perfection.Deficient)
    assert(Perfect.perfection(10) === Perfection.Deficient)
    assert(Perfect.perfection(12) === Perfection.Abundant)
    assert(Perfect.perfection(16) === Perfection.Deficient)
    assert(Perfect.perfection(24) === Perfection.Abundant)
    assert(Perfect.perfection(28) === Perfection.Perfect)
  }
}
