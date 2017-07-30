package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPythagorean extends FunSuite {
  test("pythagoreanC returns Option(c) so that c is a pythagorean match to a and b and a + b + c add up to sum") {
    assert(Pythagorean.pythagoreanC(1, 2, 3) === None)
    assert(Pythagorean.pythagoreanC(3, 4, 12) === Some(5))
  }

  test("pythagoreanTripletBySum returns Option(a, b, c) so that they are a pythagorean triple adding up to sum") {
    assert(Pythagorean.pythagoreanTripletBySum(7) === None)
    assert(Pythagorean.pythagoreanTripletBySum(12) === Some((3, 4, 5))
      || Pythagorean.pythagoreanTripletBySum(12) === Some(4, 3, 5))
    assert(Pythagorean.pythagoreanProduct(Pythagorean.pythagoreanTripletBySum(7)) === "None")
    assert(Pythagorean.pythagoreanProduct(Pythagorean.pythagoreanTripletBySum(12)) === "60")
  }
}
