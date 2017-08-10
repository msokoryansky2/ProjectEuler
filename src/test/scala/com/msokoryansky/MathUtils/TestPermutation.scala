package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPermutation extends FunSuite {
  test("lexicographicPermutation returns ith permutation of a set of chars") {
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 3) === "1342")
  }
}
