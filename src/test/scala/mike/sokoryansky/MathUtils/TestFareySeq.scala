package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestFareySeq extends FunSuite {
  test("count returns number of fractions in farey sequence for specified denominator") {
    intercept[Exception] {
      FareySeq.count(1)
    }

    assert(FareySeq.count(2) === 1)
    assert(FareySeq.count(3) === 2)
    assert(FareySeq.count(4) === 2)
    assert(FareySeq.count(5) === 4)
    assert(FareySeq.count(6) === 2)
    assert(FareySeq.count(7) === 6)
    assert(FareySeq.count(8) === 4)
    assert(FareySeq.count(97) === 96)
  }

  test("count2ToN returns number of fractions in Farey Sequence for all denominators 2 through N") {
    intercept[Exception] {
      FareySeq.count2ToN(1)
    }

    assert(FareySeq.count2ToN(2) === 1)
    assert(FareySeq.count2ToN(3) === 3)
    assert(FareySeq.count2ToN(4) === 5)
    assert(FareySeq.count2ToN(5) === 9)
    assert(FareySeq.count2ToN(6) === 11)
    assert(FareySeq.count2ToN(7) === 17)
    assert(FareySeq.count2ToN(8) === 21)
  }

  test("numerators returns all numerators of Farey Seq fractions for specified denominator") {
    assert(FareySeq.numerators(2) === List(1))
    assert(FareySeq.numerators(3) === List(1, 2))
    assert(FareySeq.numerators(4) === List(1, 3))
    assert(FareySeq.numerators(5) === List(1, 2, 3, 4))
    assert(FareySeq.numerators(6) === List(1, 5))
    assert(FareySeq.numerators(7) === List(1, 2, 3, 4, 5, 6))
    assert(FareySeq.numerators(8) === List(1, 3, 5, 7))
  }

  test("numerators2ToN returns all numerators of all Farey Seq fractions for denominators from 2 to specified number") {
    assert(FareySeq.numerators2ToN(8) === Map(
                                                  2 -> List(1),
                                                  3 -> List(1, 2),
                                                  4 -> List(1, 3),
                                                  5 -> List(1, 2, 3, 4),
                                                  6 -> List(1, 5),
                                                  7 -> List(1, 2, 3, 4, 5, 6),
                                                  8 -> List(1, 3, 5, 7)))
  }

  test("numeratorsFilter returns all numerators of Farey Seq fractions for specified denom " +
    "that satisfy specified predicate") {
    assert(FareySeq.numeratorsFilter(2, f => f <= Fraction(1, 2)) === List(1))
    assert(FareySeq.numeratorsFilter(3, f => f <= Fraction(1, 2)) === List(1))
    assert(FareySeq.numeratorsFilter(4, f => f <= Fraction(1, 2)) === List(1))
    assert(FareySeq.numeratorsFilter(5, f => f <= Fraction(1, 2)) === List(1, 2))
    assert(FareySeq.numeratorsFilter(6, f => f <= Fraction(1, 2)) === List(1))
    assert(FareySeq.numeratorsFilter(7, f => f <= Fraction(1, 2)) === List(1, 2, 3))
    assert(FareySeq.numeratorsFilter(8, f => f <= Fraction(1, 2)) === List(1, 3))
  }

  test("numeratorsFilter2ToN returns all numerators of all Farey Seq fractions " +
    "that satisfy specified predicate") {
    assert(FareySeq.numeratorsFilter2ToN(8, f => f <= Fraction(1, 2)) === Map(
                                                  2 -> List(1),
                                                  3 -> List(1),
                                                  4 -> List(1),
                                                  5 -> List(1, 2),
                                                  6 -> List(1),
                                                  7 -> List(1, 2, 3),
                                                  8 -> List(1, 3)))
  }

  test("numeratorsLessThanFraction returns all numerators of Fareq Seq fractions" +
    "where such fraction is < specified comparison fraction") {
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(6, 6)) === List(1, 5, 7, 11, 13, 17))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(5, 6)) === List(1, 5, 7, 11, 13))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(4, 6)) === List(1, 5, 7, 11))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(2, 3)) === List(1, 5, 7, 11))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 2)) === List(1, 5, 7))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 3)) === List(1, 5))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 4)) === List(1))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 5)) === List(1))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 17)) === List(1))
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 18)) === List())
    assert(FareySeq.numeratorsLessThanFraction(18, Fraction(1, 19)) === List())

    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(6, 6)) === List(1, 2, 4, 5, 8, 10, 11, 13, 16, 17, 19, 20))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(5, 6)) === List(1, 2, 4, 5, 8, 10, 11, 13, 16, 17))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(4, 6)) === List(1, 2, 4, 5, 8, 10, 11, 13))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(2, 3)) === List(1, 2, 4, 5, 8, 10, 11, 13))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(3, 8)) === List(1, 2, 4, 5))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 2)) === List(1, 2, 4, 5, 8, 10))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 3)) === List(1, 2, 4, 5))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 4)) === List(1, 2, 4, 5))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 5)) === List(1, 2, 4))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 6)) === List(1, 2))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 7)) === List(1, 2))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 10)) === List(1, 2))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 11)) === List(1))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 20)) === List(1))
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 21)) === List())
    assert(FareySeq.numeratorsLessThanFraction(21, Fraction(1, 22)) === List())
  }

  test("numeratorsLessThanFraction2ToN returns map from denoms between 2 and specified number to numerators of " +
    "Fareq Seq fractions where such fraction is < specified comparison fraction") {
    assert(FareySeq.numeratorsLessThanFraction2ToN(8, Fraction(1, 2)) === Map(3 -> List(1),
                                                                              4 -> List(1),
                                                                              5 -> List(1, 2),
                                                                              6 -> List(1),
                                                                              7 -> List(1, 2, 3),
                                                                              8 -> List(1, 3)))
    assert(FareySeq.numeratorsLessThanFraction2ToN(8, Fraction(1, 3)) === Map(4 -> List(1),
                                                                              5 -> List(1),
                                                                              6 -> List(1),
                                                                              7 -> List(1, 2),
                                                                              8 -> List(1)))
  }
}
