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
    assert(StarsAndBars.count(4, 2) === 3)
    assert(StarsAndBars.count(20, 10) === 92378)
  }

  test("countAllBars counts all potential numbers of ways bars from 0 (or 1 if zeroBar == false) to (stars - 1)" +
    "can be fit between stars") {
    assert(StarsAndBars.countAllBars(2) === 2)
    assert(StarsAndBars.countAllBars(2, zeroBar = false) === 1)
    assert(StarsAndBars.countAllBars(4) === 8)
    assert(StarsAndBars.countAllBars(4, zeroBar = false) === 7)
    assert(StarsAndBars.countAllBars(10) === 512)
    assert(StarsAndBars.countAllBars(10, zeroBar = false) === 511)
  }

  /**
    * --------------------------------------------------------------------------------------------------------------
    * Tests for StarsAndBars logic that treats sum of (3 + 4) as being indistinguishable from sum of (4 + 3).
    * This algo for this is wrong and needs to be fixed.
    *
    * TODO: uncomment and add to these tests when associated logic is fixed
    * --------------------------------------------------------------------------------------------------------------
    *
  test("countDistinctAllBars counts all potential distinct sums of stars -- (3 + 4) is the same as (3 + 4)") {
    assert(StarsAndBars.countDistinctAllBars(2, zeroBar = false) === 1)
    assert(StarsAndBars.countDistinctAllBars(3, zeroBar = false) === 2)
    assert(StarsAndBars.countDistinctAllBars(4, zeroBar = false) === 4)
    assert(StarsAndBars.countDistinctAllBars(5, zeroBar = false) === 6)
    assert(StarsAndBars.countDistinctAllBars(6, zeroBar = false) === 10)
    assert(StarsAndBars.countDistinctAllBars(7, zeroBar = false) === 14)
    assert(StarsAndBars.countDistinctAllBars(8, zeroBar = false) === 21)
    assert(StarsAndBars.countDistinctAllBars(9, zeroBar = false) === 29)
    assert(StarsAndBars.countDistinctAllBars(10, zeroBar = false) === 41)
    assert(StarsAndBars.countDistinctAllBars(11, zeroBar = false) === 55)
    assert(StarsAndBars.countDistinctAllBars(12, zeroBar = false) === 76)
    assert(StarsAndBars.countDistinctAllBars(13, zeroBar = false) === 100)
    assert(StarsAndBars.countDistinctAllBars(14, zeroBar = false) === 134)
    assert(StarsAndBars.countDistinctAllBars(15, zeroBar = false) === 175)
  }
    */
}
