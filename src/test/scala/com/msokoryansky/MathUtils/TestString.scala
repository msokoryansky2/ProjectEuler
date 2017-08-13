package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestString extends FunSuite {
  test("rotate shifts string characters") {
    import com.msokoryansky.MathUtils.StringOps.StringUtilityOps
    assert("blah".rotate(0) === "blah")
    assert("blah".rotate(1) === "hbla")
    assert("blah".rotate(2) === "ahbl")
    assert("blah".rotate(3) === "lahb")
    assert("blah".rotate(4) === "blah")
    assert("blah".rotate(5) === "hbla")
    assert("blah".rotate(17) === "hbla")
    assert("blah".rotate(-1) === "lahb")
    assert("blah".rotate(-2) === "ahbl")
    assert("blah".rotate(-3) === "hbla")
    assert("blah".rotate(-4) === "blah")
    assert("blah".rotate(-5) === "lahb")
    assert("blah".rotate(-17) === "lahb")
  }
}
