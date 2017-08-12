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

  test("SumOfParts finds all ways to add multiples of various parts to equal to specified sum") {
    assert(SumOfParts.waysToAddUpParts(3, Set(1, 3)) == Set(Map(1 -> 3, 3 -> 0), Map(1 -> 0, 3 -> 1)))
    assert(SumOfParts.waysToAddUpParts(3, Set(1, 2)) == Set(Map(1 -> 3, 2 -> 0), Map(1 -> 1, 2 -> 1)))
    assert(SumOfParts.waysToAddUpParts(5, Set(1, 3, 4, 7)) ==
      Set(Map(1 -> 5, 3 -> 0, 4 -> 0, 7 -> 0),
          Map(1 -> 2, 3 -> 1, 4 -> 0, 7 -> 0),
          Map(1 -> 1, 3 -> 0, 4 -> 1, 7 -> 0)))
    assert(SumOfParts.waysToAddUpParts(13, Set(2, 3, 5)) ==
      Set(Map(2 -> 5, 3 -> 1, 5 -> 0),
          Map(2 -> 4, 3 -> 0, 5 -> 1),
          Map(2 -> 2, 3 -> 3, 5 -> 0),
          Map(2 -> 1, 3 -> 2, 5 -> 1),
          Map(2 -> 0, 3 -> 1, 5 -> 2)))
  }
}
