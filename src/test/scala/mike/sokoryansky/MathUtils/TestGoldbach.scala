package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestGoldbach extends FunSuite {
  test("goldbachCandidates is a stream of odd, non-prime numbers") {
    assert(Goldbach.goldbachCandidates.take(8).toList === List(9, 15, 21, 25, 27, 33, 35, 39))
  }

  test("goldbachFactor finds the number to be squared and doubled + a prime to add up to odd, non-prime number") {
    assert(Goldbach.goldbachFactor(9) === Some(1, 7))
    assert(Goldbach.goldbachFactor(21) === Some(1, 19))
    assert(Goldbach.goldbachFactor(27) === Some(2, 19))
    assert(Goldbach.goldbachFactor(39) === Some(1, 37))
  }
}
