package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestSqrt extends FunSuite {

  test("sqrtOf2Expansion returns expansion of sqrt(2) in (numerator, denominator) form") {
    assert(Sqrt.sqrtOf2Fraction(1) === (HugePositiveInt(3), HugePositiveInt(2)))
    assert(Sqrt.sqrtOf2Fraction(2) === (HugePositiveInt(7), HugePositiveInt(5)))
    assert(Sqrt.sqrtOf2Fraction(3) === (HugePositiveInt(17), HugePositiveInt(12)))
    assert(Sqrt.sqrtOf2Fraction(4) === (HugePositiveInt(41), HugePositiveInt(29)))
    assert(Sqrt.sqrtOf2Fraction(5) === (HugePositiveInt(99), HugePositiveInt(70)))
    assert(Sqrt.sqrtOf2Fraction(6) === (HugePositiveInt(239), HugePositiveInt(169)))
    assert(Sqrt.sqrtOf2Fraction(7) === (HugePositiveInt(577), HugePositiveInt(408)))
    assert(Sqrt.sqrtOf2Fraction(8) === (HugePositiveInt(1393), HugePositiveInt(985)))

    assert(Sqrt.sqrtOf2Fraction(2, Some(Sqrt.sqrtOf2Fraction(1, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(2))
    assert(Sqrt.sqrtOf2Fraction(3, Some(Sqrt.sqrtOf2Fraction(2, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(3))
    assert(Sqrt.sqrtOf2Fraction(4, Some(Sqrt.sqrtOf2Fraction(3, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(4))
    assert(Sqrt.sqrtOf2Fraction(5, Some(Sqrt.sqrtOf2Fraction(4, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(5))
    assert(Sqrt.sqrtOf2Fraction(6, Some(Sqrt.sqrtOf2Fraction(5, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(6))
    assert(Sqrt.sqrtOf2Fraction(7, Some(Sqrt.sqrtOf2Fraction(6, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(7))
    assert(Sqrt.sqrtOf2Fraction(8, Some(Sqrt.sqrtOf2Fraction(7, None, fractionOnly = true))) === Sqrt.sqrtOf2Fraction(8))
  }

  test("sqrtOf2Fractions returns a map of all sqrt(2) expansions keyed by their iterations") {
    (1 to 10).foreach(i =>
      assert(Sqrt.sqrtOf2Fractions(i) === (1 to i).map(j => (j, Sqrt.sqrtOf2Fraction(j))).toMap
    ))
  }
}
