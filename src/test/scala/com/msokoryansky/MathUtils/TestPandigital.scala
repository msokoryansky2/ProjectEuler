package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPandigital extends FunSuite {
  test("combinedPandigital1To9 tests if List[Int] is pansdigital over 1 through 9") {
    assert(Pandigital.combinedPandigital1To9(List(1, 2, 3, 45, 6789)))
    assert(Pandigital.combinedPandigital1To9(List(912, 834, 765)))
    assert(!Pandigital.combinedPandigital1To9(List(0, 1, 2, 3, 45, 6789)))
    assert(!Pandigital.combinedPandigital1To9(List(1, 2, 3, 45, 679)))
  }

  test("") {
    assert(!Pandigital.multiMultiProductPandigital1To9(12345))
    assert(Pandigital.multiMultiProductPandigital1To9(7254))
  }
}
