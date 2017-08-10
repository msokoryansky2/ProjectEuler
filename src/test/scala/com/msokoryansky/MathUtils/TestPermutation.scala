package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPermutation extends FunSuite {
  test("lexicographicPermutation returns ith permutation of a set of chars") {
    intercept[Exception] {
      Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), -1)
    }
    intercept[Exception] {
      Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 24)
    }

    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 0) === "1234")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 1) === "1243")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 2) === "1324")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 3) === "1342")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 4) === "1423")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 5) === "1432")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 6) === "2134")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 7) === "2143")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 8) === "2314")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 9) === "2341")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 10) === "2413")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 11) === "2431")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 12) === "3124")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 13) === "3142")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 14) === "3214")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 15) === "3241")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 16) === "3412")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 17) === "3421")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 18) === "4123")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 19) === "4132")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 20) === "4213")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 21) === "4231")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 22) === "4312")
    assert(Permutation.lexicographicPermutation(Set('1', '2', '3', '4'), 23) === "4321")
    assert(Permutation.lexicographicPermutation(Set('0', '1', '2', '3', '4'), 0) === "01234")
    assert(Permutation.lexicographicPermutation(Set('0', '1', '2', '3', '4'), 119) === "43210")
  }
}
