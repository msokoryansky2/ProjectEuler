package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPythagorean extends FunSuite {
  // Know non-trivial pythagorean triples for C <= 300 from https://en.wikipedia.org/wiki/Pythagorean_triple
  private val upToCOf300KnownNonTrivial = Set[(Long, Long, Long)](
    (3, 4, 5), (5, 12, 13), (8, 15, 17), (7, 24, 25), (20, 21, 29), (12, 35, 37), (9, 40, 41), (28, 45, 53),
    (11, 60, 61), (16, 63, 65), (33, 56, 65), (48, 55, 73), (13, 84, 85), (36, 77, 85), (39, 80, 89), (65, 72, 97),
    (20, 99, 101), (60, 91, 109), (15, 112, 113), (44, 117, 125), (88, 105, 137), (17, 144, 145), (24, 143, 145),
    (51, 140, 149), (85, 132, 157), (119, 120, 169), (52, 165, 173), (19, 180, 181), (57, 176, 185), (104, 153, 185),
    (95, 168, 193), (28, 195, 197), (84, 187, 205), (133, 156, 205), (21, 220, 221), (140, 171, 221),
    (60, 221, 229), (105, 208, 233), (120, 209, 241), (32, 255, 257), (23, 264, 265), (96, 247, 265), (69, 260, 269),
    (115, 252, 277), (160, 231, 281), (161, 240, 289), (68, 285, 293))

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

  test("pythagoreanTriplesPrimitive1ToN returns all integer-sides right triangles for all perimeters upto n") {
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(10) ===
      Map())
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(12) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(23) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(24) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(25) ===
      Map(12 -> List((3, 4, 5))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(30) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(31) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(36) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(39) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(40) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(45) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(50) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(52) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(55) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(56) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(58) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))
    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(60) ===
      Map(12 -> List((3, 4, 5)), 30 -> List((5, 12, 13)), 40 -> List((8, 15, 17)), 56 -> List((7, 24, 25))))

    assert(Pythagorean.pythagoreanTriplesPrimitive1ToN(900).flatMap(_._2).filter(_._3 <= 300).toSet
      === upToCOf300KnownNonTrivial)

    assert(Pythagorean.pythagoreanTriplesPrimitive1ToNSlow(900).flatMap(_._2).filter(_._3 <= 300).toSet
      === upToCOf300KnownNonTrivial)
  }

  test("pythagoreanTriples1ToN returns either all or primitive Pythagorean triples for all perimeters upto n") {
    val allTriples = Pythagorean.pythagoreanTriples1ToN(10000)
    (1 to 10000).foreach(n => {
      val nTriples = Pythagorean.pythagoreanTriples(n)
      assert((!allTriples.contains(n) && nTriples.isEmpty) || (allTriples(n).sorted === nTriples.sorted))
    })
  }
}
