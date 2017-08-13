package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestFraction extends FunSuite {
  test("Fraction toString") {
    intercept[Exception] {
      Fraction(1, 0)
    }
    assert(Fraction(32,45).toString === "32/45")
    assert(Fraction(0,45).toString === "0/45")
    assert(Fraction(-2,-3).toString === "-2/-3")
  }

  test("isWhole checks if fraction represents an integer") {
    assert(Fraction(0, 5).isWhole)
    assert(Fraction(0, -5).isWhole)
    assert(Fraction(1, 1).isWhole)
    assert(Fraction(-1, 1).isWhole)
    assert(Fraction(-1, -1).isWhole)
    assert(Fraction(1, -1).isWhole)
    assert(Fraction(10, 5).isWhole)
    assert(Fraction(-10, 5).isWhole)
    assert(Fraction(-10, -5).isWhole)
    assert(Fraction(10, -5).isWhole)
  }

  test("simplify simplifies fraction by dividing numberator and denominators by their common factors") {
    assert(Fraction(37, 21).simplify.toString === Fraction(37, 21).toString)
    assert(Fraction(24, 12).simplify.toString === Fraction(2, 1).toString)
    assert(Fraction(144, -72).simplify.toString === Fraction(-2, 1).toString)
    assert(Fraction(-144, 72).simplify.toString === Fraction(-2, 1).toString)
    assert(Fraction(768, 512).simplify.toString === Fraction(3, 2).toString)
    assert(Fraction(768, -512).simplify.toString === Fraction(-3, 2).toString)
    assert(Fraction(-768, 512).simplify.toString === Fraction(-3, 2).toString)
    assert(Fraction(-768, -512).simplify.toString === Fraction(3, 2).toString)
  }
}
