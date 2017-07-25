package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0008 extends FunSuite {
  test("P8.greatestProduct returns largest product of consecutive digits in a string representation of a number") {
    val p8 = new P0008
    assert(p8.greatestProduct("123456", 3) === 120)
    assert(p8.greatestProduct("123456", 7) === 0)
    assert(p8.greatestProduct("123", 3) === 6)
    assert(p8.greatestProduct("9999022222", 5) === 32)
    assert(p8.greatestProduct("949596", 3) === 405)
  }
}
