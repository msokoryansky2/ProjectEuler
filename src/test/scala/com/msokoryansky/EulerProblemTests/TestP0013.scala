package com.msokoryansky.EulerProblemTests

import com.msokoryansky.EulerProblems._
import org.scalatest.FunSuite

class TestP0013 extends FunSuite {
  val p = new P0013

  test("BiggieInt allows + operation on long integers represented as strings") {
    assert(new BiggieInt("abc1234dsfhdh335").biggie === "1234335")
    assert(new BiggieInt("asjewe").biggie === "0")
    assert(new BiggieInt("").biggie === "0")

    assert((new BiggieInt("23") + new BiggieInt("53")).biggie === "76")
  }
}