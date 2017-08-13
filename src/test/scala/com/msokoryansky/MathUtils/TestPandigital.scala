package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPandigital extends FunSuite {
  test("combinedPandigital1To9 tests if List[Int] is pansdigital over 1 through 9") {
    assert(Pandigital.combinedPandigital1To9(List(1, 2, 3, 45, 6789)))
    assert(Pandigital.combinedPandigital1To9(List(912, 834, 765)))
    assert(!Pandigital.combinedPandigital1To9(List(0, 1, 2, 3, 45, 6789)))
    assert(!Pandigital.combinedPandigital1To9(List(1, 2, 3, 45, 679)))
  }

  test("multiMultiProductPandigital1To9 tests if a number has two multiples that altogether form a 1-9 pandigital") {
    assert(!Pandigital.multiMultiProductPandigital1To9(12345))
    assert(Pandigital.multiMultiProductPandigital1To9(7254))
  }

  test("concatenatedMultiplePandigital1To9 tests if a number can be multiplied by a range (1 to n) which forms" +
    " a 1-9 pandigital when concatenated") {
    assert(Pandigital.concatenatedMultiplePandigital1To9(192, 1 to 3))
    assert(!Pandigital.concatenatedMultiplePandigital1To9(191, 1 to 3))
  }

  test("concatenatedMultiplePandigital1To9RangeFinder finds number n for (1 to n) range, if exists, " +
    "that makes a number concatenatedMultiplePandigital1To9-ish") {
    assert(Pandigital.concatenatedMultiplePandigital1To9RangeFinder(192) === Some(3))
    assert(Pandigital.concatenatedMultiplePandigital1To9RangeFinder(9) === Some(5))
    assert(Pandigital.concatenatedMultiplePandigital1To9RangeFinder(90) === None)
  }
}
