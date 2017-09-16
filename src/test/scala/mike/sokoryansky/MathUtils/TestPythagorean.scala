package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPythagorean extends FunSuite {
  test("rightTriangles returns all integer-sides right triangles for a given perimeter") {
    assert(Pythagorean.pythagoreanTriples(-2) === List())
    assert(Pythagorean.pythagoreanTriples(0) === List())
    assert(Pythagorean.pythagoreanTriples(1) === List())
    assert(Pythagorean.pythagoreanTriples(2) === List())
    assert(Pythagorean.pythagoreanTriples(3) === List())
    assert(Pythagorean.pythagoreanTriples(4) === List())
    assert(Pythagorean.pythagoreanTriples(5) === List())
    assert(Pythagorean.pythagoreanTriples(6) === List())
    assert(Pythagorean.pythagoreanTriples(7) === List())
    assert(Pythagorean.pythagoreanTriples(8) === List())
    assert(Pythagorean.pythagoreanTriples(9) === List())
    assert(Pythagorean.pythagoreanTriples(10) === List())
    assert(Pythagorean.pythagoreanTriples(11) === List())
    assert(Pythagorean.pythagoreanTriples(12) === List((3, 4, 5)))
    assert(Pythagorean.pythagoreanTriples(13) === List())
    assert(Pythagorean.pythagoreanTriples(14) === List())
    assert(Pythagorean.pythagoreanTriples(15) === List())
    assert(Pythagorean.pythagoreanTriples(16) === List())
    assert(Pythagorean.pythagoreanTriples(17) === List())
    assert(Pythagorean.pythagoreanTriples(18) === List())
    assert(Pythagorean.pythagoreanTriples(19) === List())
    assert(Pythagorean.pythagoreanTriples(20) === List())
    assert(Pythagorean.pythagoreanTriples(21) === List())
    assert(Pythagorean.pythagoreanTriples(22) === List())
    assert(Pythagorean.pythagoreanTriples(23) === List())
    assert(Pythagorean.pythagoreanTriples(24) === List((6, 8, 10)))
    assert(Pythagorean.pythagoreanTriples(25) === List())
    assert(Pythagorean.pythagoreanTriples(26) === List())
    assert(Pythagorean.pythagoreanTriples(27) === List())
    assert(Pythagorean.pythagoreanTriples(28) === List())
    assert(Pythagorean.pythagoreanTriples(29) === List())
    assert(Pythagorean.pythagoreanTriples(30) === List((5, 12, 13)))
    assert(Pythagorean.pythagoreanTriples(31) === List())
    assert(Pythagorean.pythagoreanTriples(32) === List())
    assert(Pythagorean.pythagoreanTriples(33) === List())
    assert(Pythagorean.pythagoreanTriples(34) === List())
    assert(Pythagorean.pythagoreanTriples(35) === List())
    assert(Pythagorean.pythagoreanTriples(36) === List((9, 12, 15)))
    assert(Pythagorean.pythagoreanTriples(37) === List())
    assert(Pythagorean.pythagoreanTriples(38) === List())
    assert(Pythagorean.pythagoreanTriples(39) === List())
    assert(Pythagorean.pythagoreanTriples(40) === List((8, 15, 17)))
    assert(Pythagorean.pythagoreanTriples(41) === List())
    assert(Pythagorean.pythagoreanTriples(42) === List())
    assert(Pythagorean.pythagoreanTriples(43) === List())
    assert(Pythagorean.pythagoreanTriples(44) === List())
    assert(Pythagorean.pythagoreanTriples(45) === List())
    assert(Pythagorean.pythagoreanTriples(46) === List())
    assert(Pythagorean.pythagoreanTriples(47) === List())
    assert(Pythagorean.pythagoreanTriples(48) === List((12, 16, 20)))
    assert(Pythagorean.pythagoreanTriples(50) === List())
    assert(Pythagorean.pythagoreanTriples(51) === List())
    assert(Pythagorean.pythagoreanTriples(52) === List())
    assert(Pythagorean.pythagoreanTriples(53) === List())
    assert(Pythagorean.pythagoreanTriples(54) === List())
    assert(Pythagorean.pythagoreanTriples(55) === List())
    assert(Pythagorean.pythagoreanTriples(56) === List((7, 24, 25)))
    assert(Pythagorean.pythagoreanTriples(60) === List((10, 24, 26), (15, 20, 25)))
    assert(Pythagorean.pythagoreanTriples(120) === List((20,48,52), (24,45,51), (30,40,50)))
    assert(Pythagorean.pythagoreanTriples(646) === List((68, 285, 293)))
  }

  test("pythagoreanTriplesNonTrivial1ToN returns all integer-sides right triangles for all perimeters upto n") {
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(-2) ===
      Map())
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(0) ===
      Map())
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(1) ===
      Map())
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(12) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(23) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(24) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(25) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(30) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(31) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(36) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(39) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(40) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(45) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(50) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(52) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(55) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(56) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(58) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))
    assert(Pythagorean.pythagoreanTriplesNonTrivial1ToN(60) ===
      Map(12 ->  List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))
  }
}
