package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

/*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
*/

class TestCollatz extends FunSuite {
  test("collatz should generate Collatz sequence for any positive integer") {
    intercept[Exception] {
      assert(Collatz.collatz(0) === List(1))
    }
    assert(Collatz.collatz(1) === List(1))
    assert(Collatz.collatz(2) === List(2, 1))
    assert(Collatz.collatz(3) === List(3, 10, 5, 16, 8, 4, 2, 1))
    assert(Collatz.collatz(4) === List(4, 2, 1))
    assert(Collatz.collatz(5) === List(5, 16, 8, 4, 2, 1))
    assert(Collatz.collatz(13) === List(13, 40, 20, 10, 5, 16, 8, 4, 2, 1))
  }

  test("longestCollatz returns number in specified range that has longest collatz sequence") {
    assert(Collatz.longestCollatz(1, 6) === 3)
    assert(Collatz.longestCollatz(4, 6) === 5)
  }
}
