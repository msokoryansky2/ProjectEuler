package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP9 extends FunSuite {
  test("P9.pythagoreanC returns Option(c) so that c is a pythagorean match to a and b and a + b + c add up to sum") {
    val p9 = new P9
    assert(p9.pythagoreanC(1, 2, 3) === None)
    assert(p9.pythagoreanC(3, 4, 12) === Some(5))
  }

  test("P9.pythagoreanTripletBySum returns Option(a, b, c) so that they are a pythagorean triple adding up to sum") {
    val p9 = new P9
    assert(p9.pythagoreanTripletBySum(7) === None)
    assert(p9.pythagoreanTripletBySum(12) === Some((3, 4, 5)) || p9.pythagoreanTripletBySum(12) === Some(4, 3, 5))
    assert(p9.pythagoreanProduct(p9.pythagoreanTripletBySum(7)) === "None")
    assert(p9.pythagoreanProduct(p9.pythagoreanTripletBySum(12)) === "60")
  }
}
