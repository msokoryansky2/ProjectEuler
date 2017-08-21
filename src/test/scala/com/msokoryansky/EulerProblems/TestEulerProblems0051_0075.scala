package com.msokoryansky.EulerProblems

import org.scalatest.FunSuite

class TestEulerProblems0051_0075 extends FunSuite {
  test("#51") {
    assert((new P0051).run === "121313")
  }

  test("#52") {
    assert((new P0052).run === "142857")
  }

  test("#53") {
    assert((new P0053).run === "4075")
  }

  test("#54") {
    assert((new P0054).run === "376")
  }

  test("#55") {
    assert((new P0055).run === "249")
  }

  test("#56") {
    assert((new P0056).run === "972")
  }

  test("#57") {
    assert((new P0057).run === "153")
  }

  test("#58") {
    assert((new P0058).run === "26241")
  }

  test("#59") {
    assert((new P0059).run === "107359")
  }
}
