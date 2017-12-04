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

  test("sqrt() returns string representation of square root to specified number of decimal places") {
    assert(Sqrt.sqrt("0", 0) === ("0", ""))
    assert(Sqrt.sqrt("0", 20) === ("0", ""))
    assert(Sqrt.sqrt("1", 0) === ("1", ""))
    assert(Sqrt.sqrt("1", 10) === ("1", "0000000000"))
    assert(Sqrt.sqrt("9", 0) === ("3", ""))
    assert(Sqrt.sqrt("9", 3) === ("3", "000"))
    // sqrt(2) = 1.41421356237309504880 (to 20 decimal places)
    assert(Sqrt.sqrt("2", 0) === ("1", ""))
    assert(Sqrt.sqrt("2", 1) === ("1", "4"))
    assert(Sqrt.sqrt("2", 2) === ("1", "41"))
    assert(Sqrt.sqrt("2", 3) === ("1", "414"))
    assert(Sqrt.sqrt("2", 5) === ("1", "41421"))
    assert(Sqrt.sqrt("2", 10) === ("1", "4142135623"))
    assert(Sqrt.sqrt("2", 20) === ("1", "41421356237309504880"))
    assert(Sqrt.sqrt("2", 100) === ("1", "4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727"))
    assert(Sqrt.sqrt("2", 200) === ("1", "41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147"))
  }

  test("sqrtFast() is same as sqrt() but hopefully faster") {
    assert(Sqrt.sqrtFast("0", 0) === ("0", ""))
    assert(Sqrt.sqrtFast("0", 20) === ("0", ""))
    assert(Sqrt.sqrtFast("1", 0) === ("1", ""))
    assert(Sqrt.sqrtFast("1", 10) === ("1", "0000000000"))
    assert(Sqrt.sqrtFast("9", 0) === ("3", ""))
    assert(Sqrt.sqrtFast("9", 3) === ("3", "000"))
    // sqrt(2) = 1.41421356237309504880 (to 20 decimal places)
    assert(Sqrt.sqrtFast("2", 0) === ("1", ""))
    assert(Sqrt.sqrtFast("2", 1) === ("1", "4"))
    assert(Sqrt.sqrtFast("2", 2) === ("1", "41"))
    assert(Sqrt.sqrtFast("2", 3) === ("1", "414"))
    assert(Sqrt.sqrtFast("2", 5) === ("1", "41421"))
    assert(Sqrt.sqrtFast("2", 10) === ("1", "4142135623"))
    assert(Sqrt.sqrtFast("2", 20) === ("1", "41421356237309504880"))
    assert(Sqrt.sqrtFast("2", 100) === ("1", "4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727"))
    assert(Sqrt.sqrtFast("2", 200) === ("1", "41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147"))
  }
}
