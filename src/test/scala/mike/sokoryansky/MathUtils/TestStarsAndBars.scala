package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestStarsAndBars extends FunSuite {
  test("count counts all possible (non-distinct sums) bars placements for specified number of stars and bars") {
    intercept[Exception] {
      StarsAndBars.count(0, 0)
    }
    intercept[Exception] {
      StarsAndBars.count(0, 1)
    }
    intercept[Exception] {
      StarsAndBars.count(1, 1)
    }
    intercept[Exception] {
      StarsAndBars.count(3, 3)
    }
    intercept[Exception] {
      StarsAndBars.count(-3, 1)
    }
    assert(StarsAndBars.count(1, 0) === 1)
    assert(StarsAndBars.count(2, 0) === 1)
    assert(StarsAndBars.count(10, 0) === 1)
    assert(StarsAndBars.count(2, 1) === 1)
    assert(StarsAndBars.count(10, 1) === 9)
    assert(StarsAndBars.count(3, 2) === 1)
    assert(StarsAndBars.count(4, 2) === 6)
    assert(StarsAndBars.count(20, 10) === 6)
  }
}
