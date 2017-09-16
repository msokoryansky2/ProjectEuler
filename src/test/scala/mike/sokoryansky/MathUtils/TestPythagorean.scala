package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPythagorean extends FunSuite {

  test("rightTriangles returns all integer-sides right triangles for a given perimeter") {
    assert(Pythagorean.pythagoreanTriple(-2) === List())
    assert(Pythagorean.pythagoreanTriple(0) === List())
    assert(Pythagorean.pythagoreanTriple(5) === List())
    assert(Pythagorean.pythagoreanTriple(11) === List())
    assert(Pythagorean.pythagoreanTriple(12) === List((3, 4, 5)))
    assert(Pythagorean.pythagoreanTriple(13) === List())
    assert(Pythagorean.pythagoreanTriple(24) === List((6, 8, 10)))
    assert(Pythagorean.pythagoreanTriple(120) === List((20,48,52), (24,45,51), (30,40,50)))
  }
}
