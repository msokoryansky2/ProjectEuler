package com.msokoryansky.MathUtils

import org.scalatest.FunSuite

class TestPoker extends FunSuite {
  test("Can convert cards to/from strings") {
    intercept[Exception] {
      Card("4E")
    }
    intercept[Exception] {
      Card("WD")
    }
    assert(Card("5D").toString === "5D")
  }

  test("Can convert poker hands to/from strings") {
    intercept[Exception] {
      Hand("5H 5C 6S 7SS KD")
    }
    intercept[Exception] {
      Hand("5H 5C 6Q 7SS KD")
    }
    assert(Hand("5H 5C 6S 7S KD").toString === "5H 5C 6S 7S KD")
  }

  test("Can compare two poker hands") {
    assert(Hand("5H 5C 6S 7S KD") < Hand("2C 3S 8S 8D TD"))
    assert(Hand("5D 8C 9S JS AC") > Hand("2C 5C 7D 8S QH"))
    assert(Hand("2D 9C AS AH AC") < Hand("3D 6D 7D TD QD"))
    assert(Hand("4D 6S 9H QH QC") > Hand("3D 6D 7H QD QS"))
    assert(Hand("2H 2D 4C 4D 4S") > Hand("3C 3D 3S 9S 9D"))
  }
}
