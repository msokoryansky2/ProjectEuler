package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestAmicable extends FunSuite {
  test("isAmicable checks if a pair of numbers is amicable") {
    assert(Amicable.isAmicable(10, 10) === false)
    assert(Amicable.isAmicable(121, 255) === false)
    assert(Amicable.isAmicable(220, 284))
    assert(Amicable.isAmicable(284, 220))
  }
}
