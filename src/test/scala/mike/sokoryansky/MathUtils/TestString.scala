package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestString extends FunSuite {
  test("rotate shifts string characters") {
    import mike.sokoryansky.MathUtils.StringOps.StringUtilityOps
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

  test("letterValue returns numeric value of letters relative to 'A' which is considered 1") {
    assert(String.letterValue('A') === 1)
    assert(String.letterValue('C') === 3)
    assert(String.letterValue('Z') === 26)
    assert(String.letterValue('a') === 1)
    assert(String.letterValue('c') === 3)
    assert(String.letterValue('z') === 26)
  }

  test("wordValue returns numeric value of a string by adding up values of all its letters") {
    assert(String.wordValue("A") === 1)
    assert(String.wordValue("CAB") === 6)
    assert(String.wordValue("ZZZ") === 78)
    assert(String.wordValue("a") === 1)
    assert(String.wordValue("Cab") === 6)
    assert(String.wordValue("zzZ") === 78)
  }
}
