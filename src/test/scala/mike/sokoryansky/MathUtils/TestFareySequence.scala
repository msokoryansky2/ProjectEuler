package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestFareySequence extends FunSuite {
  test("count returns number of fractions in farey sequence for specified denominator") {
    intercept[Exception] {
      FareySequence.count(1)
    }

    assert(FareySequence.count(2) === 1)
    assert(FareySequence.count(3) === 2)
    assert(FareySequence.count(4) === 2)
    assert(FareySequence.count(5) === 4)
    assert(FareySequence.count(6) === 2)
    assert(FareySequence.count(7) === 6)
    assert(FareySequence.count(8) === 4)
    assert(FareySequence.count(97) === 96)
  }

  test("count2ToN returns number of fractions in Farey Sequence for all denominators 2 through N") {
    intercept[Exception] {
      FareySequence.count2ToN(1)
    }

    assert(FareySequence.count2ToN(2) === 1)
    assert(FareySequence.count2ToN(3) === 3)
    assert(FareySequence.count2ToN(4) === 5)
    assert(FareySequence.count2ToN(5) === 9)
    assert(FareySequence.count2ToN(6) === 11)
    assert(FareySequence.count2ToN(7) === 17)
    assert(FareySequence.count2ToN(8) === 21)
  }
}
