package mike.sokoryansky.MathUtils

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

  test("generate finds all ways to add multiples of various parts to equal to specified sum") {
    assert(SumsOfParts.generate(3, Set(1, 3)) == Set(Map(1 -> 3, 3 -> 0), Map(1 -> 0, 3 -> 1)))
    assert(SumsOfParts.generate(3, Set(1, 2)) == Set(Map(1 -> 3, 2 -> 0), Map(1 -> 1, 2 -> 1)))
    assert(SumsOfParts.generate(5, Set(1, 3, 4, 7)) ==
      Set(Map(1 -> 5, 3 -> 0, 4 -> 0, 7 -> 0),
          Map(1 -> 2, 3 -> 1, 4 -> 0, 7 -> 0),
          Map(1 -> 1, 3 -> 0, 4 -> 1, 7 -> 0)))
    assert(SumsOfParts.generate(13, Set(2, 3, 5)) ==
      Set(Map(2 -> 5, 3 -> 1, 5 -> 0),
          Map(2 -> 4, 3 -> 0, 5 -> 1),
          Map(2 -> 2, 3 -> 3, 5 -> 0),
          Map(2 -> 1, 3 -> 2, 5 -> 1),
          Map(2 -> 0, 3 -> 1, 5 -> 2)))
  }

  test("generateAsSumToString pretty prints result of generate as a sum") {
    val r = SumsOfParts.generate(13, Set(2, 3, 5))
    assert(SumsOfParts.generateAsSumToString(r).trim ===
        """
          |5 + 5 + 3
          |5 + 3 + 3 + 2
          |5 + 2 + 2 + 2 + 2
          |3 + 3 + 3 + 2 + 2
          |3 + 2 + 2 + 2 + 2 + 2
        """.stripMargin.trim)
  }

  test("count counts all all ways to add multiples of various parts to equal to specified sum without listing them") {
    assert(SumsOfParts.count(3, Seq(1, 2)) === 2)
    assert(SumsOfParts.count(3, Seq(1, 2, 3)) === 2)
    assert(SumsOfParts.count(5, Seq(1, 3, 4, 7)) === 3)
    assert(SumsOfParts.count(13, Seq(2, 3, 5)) === 5)
    assert(SumsOfParts.count(5, Seq(1, 2, 3, 4)) === 6)
    assert(SumsOfParts.count(5, Seq(1, 2, 3, 4, 5)) === 6)
    assert(SumsOfParts.count(6, Seq(1, 2, 3, 4, 5)) === 10)
    assert(SumsOfParts.count(6, Seq(1, 2, 3, 4, 5, 6)) === 10)
  }

  test("count2 is like count except parts are all positive integers less than sum, which allows for optimized code") {
    assert(SumsOfParts.count(5, 1 until 5) === SumsOfParts.count2(5))
    assert(SumsOfParts.count(10, 1 until 10) === SumsOfParts.count2(10))
    assert(SumsOfParts.count(17, 1 until 17) === SumsOfParts.count2(17))
  }

  test("permutations returns all permutations of a set of chars") {
    assert(Permutation.permutations(Set('A', 'B')).sortWith(_ < _) ===
      List("AB", "BA"))
    assert(Permutation.permutations(Set('A', 'B', 'C')).sortWith(_ < _) ===
      List("ABC", "ACB", "BAC", "BCA", "CAB", "CBA"))
    assert(Permutation.permutations(Set('1', '2', '3', '4')).sortWith(_ < _) ===
      List("1234", "1243", "1324", "1342", "1423", "1432",
            "2134", "2143", "2314", "2341", "2413", "2431",
            "3124", "3142", "3214", "3241", "3412", "3421",
            "4123", "4132", "4213", "4231", "4312", "4321"))
  }

  test("permutations returns all permutations of a string") {
    assert(Permutation.permutations("AB").sortWith(_ < _) ===
      List("AB", "BA"))
    assert(Permutation.permutations("ABC").sortWith(_ < _) ===
      List("ABC", "ACB", "BAC", "BCA", "CAB", "CBA"))
    assert(Permutation.permutations("1234").sortWith(_ < _) ===
      List("1234", "1243", "1324", "1342", "1423", "1432",
        "2134", "2143", "2314", "2341", "2413", "2431",
        "3124", "3142", "3214", "3241", "3412", "3421",
        "4123", "4132", "4213", "4231", "4312", "4321"))
    assert(Permutation.permutations("1233").sortWith(_ < _) ===
      List("1233", "1323", "1332",
        "2133", "2313", "2331",
        "3123", "3132", "3213", "3231", "3312", "3321"))
  }
}
