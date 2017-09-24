package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestLatticePath extends FunSuite {
  test("numPathsSlow returns number of downward/rightward paths along edges from upper left corner of a X-by-Y lattice") {
    intercept[Exception] {
      assert(LatticePath.numPathsSlow(100, 0) === 1)
    }
    intercept[Exception] {
      assert(LatticePath.numPathsSlow(0, 100) === 1)
    }
    assert(LatticePath.numPathsSlow(1, 1) === 2)
    assert(LatticePath.numPathsSlow(2, 2) === 6)
    assert(LatticePath.numPathsSlow(3, 3) === 20)
  }

  test("numPaths returns number of downward/rightward paths along edges from upper left corner of a X-by-Y lattice") {
    intercept[Exception] {
      assert(LatticePath.numPaths(100, 0) === 1)
    }
    intercept[Exception] {
      assert(LatticePath.numPaths(0, 100) === 1)
    }
    assert(LatticePath.numPaths(1, 1) === 2)
    assert(LatticePath.numPaths(2, 2) === 6)
    assert(LatticePath.numPaths(3, 3) === 20)
  }
}
