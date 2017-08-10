package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPermutation extends FunSuite {
  test("lexicographicPermutation returns ith permutation of a set of chars") {
    intercept[Exception] {
      Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), -1)
    }
    intercept[Exception] {
      Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 120)
    }

    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 0) === "1234")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 1) === "1243")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 2) === "1324")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 3) === "1342")
    assert(Permutation.lexicographicPermutation(Set('0', '1', '2', '3', '4'), 0) === "01234")
    assert(Permutation.lexicographicPermutation(Set('0', '1', '2', '3', '4'), 119) === "43210")
  }
}
