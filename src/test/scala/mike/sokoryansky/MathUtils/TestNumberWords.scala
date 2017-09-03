package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestNumberWords extends FunSuite {
  test("word gets word representation of a number") {
    assert(NumberWords.word(0) === "zero")
    assert(NumberWords.word(73) === "seventy-three")
    assert(NumberWords.word(100) === "one hundred")
    assert(NumberWords.word(101) === "one hundred and one")
    assert(NumberWords.word(111) === "one hundred and eleven")
    assert(NumberWords.word(573) === "five hundred and seventy-three")
    assert(NumberWords.word(1000) === "one thousand")
  }
}
