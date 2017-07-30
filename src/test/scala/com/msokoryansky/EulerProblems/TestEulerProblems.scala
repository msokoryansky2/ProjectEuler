package com.msokoryansky.EulerProblems

import org.scalatest.FunSuite

class TestEulerProblems extends FunSuite {
  test("#1") {
    assert((new P0001).run === "233168")
  }

  test("#2") {
    assert((new P0002).run === "4613732")
  }

  test("#3") {
    assert((new P0003).run === "6857")
  }

  test("#4") {
    assert((new P0004).run === "906609")
  }

  test("#5") {
    assert((new P0005).run === "232792560")
  }

  test("#6") {
    assert((new P0006).run === "25164150")
  }

  test("#7") {
    assert((new P0007).run === "104743")
  }

  test("#8") {
    assert((new P0008).run === "23514624000")
  }

  test("#9") {
    assert((new P0009).run === "31875000")
  }

  test("#10") {
    assert((new P0010).run === "142913828922")
  }

  test("#11") {
    assert((new P0011).run === "70600674")
  }

  test("#12") {
    assert((new P0012).run === "76576500")
  }

  test("#13") {
    assert((new P0013).run === "5537376230")
  }
}
