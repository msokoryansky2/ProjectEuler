package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestReciprocal extends FunSuite {
  test("reciprocal returns a Decimal representation of a reciprocal fraction") {
    intercept[Exception] {
      Reciprocal.reciprocal(-1)
    }
    intercept[Exception] {
      Reciprocal.reciprocal(0)
    }
    assert(Reciprocal.reciprocal(1).toString === "1")
    assert(Reciprocal.reciprocal(2).toString === "0.5")
    assert(Reciprocal.reciprocal(3).toString === "0.(3)")
    assert(Reciprocal.reciprocal(4).toString === "0.25")
    assert(Reciprocal.reciprocal(5).toString === "0.2")
    assert(Reciprocal.reciprocal(6).toString === "0.1(6)")
    assert(Reciprocal.reciprocal(7).toString === "0.(142857)")
    assert(Reciprocal.reciprocal(8).toString === "0.125")
    assert(Reciprocal.reciprocal(9).toString === "0.(1)")
    assert(Reciprocal.reciprocal(10).toString === "0.1")
    assert(Reciprocal.reciprocal(11).toString === "0.(09)")
    assert(Reciprocal.reciprocal(12).toString === "0.08(3)")
    assert(Reciprocal.reciprocal(13).toString === "0.(076923)")
    assert(Reciprocal.reciprocal(13).toString === "0.0(714285)")
    assert(Reciprocal.reciprocal(23).toString === "0.0(0434782608695652173913)")
    assert(Reciprocal.reciprocal(25).toString === "0.04")
    assert(Reciprocal.reciprocal(36).toString === "0.02(7)")
    assert(Reciprocal.reciprocal(300).toString === "0.00(3)")
    assert(Reciprocal.reciprocal(400).toString === "0.0025")
  }
}
