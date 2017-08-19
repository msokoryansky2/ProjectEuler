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

  test("Can compare two poker hands") {
    assert(PokerHand("5H 5C 6S 7S KD") < PokerHand("2C 3S 8S 8D TD"))
    assert(PokerHand("5D 8C 9S JS AC") > PokerHand("2C 5C 7D 8S QH"))
    assert(PokerHand("2D 9C AS AH AC") < PokerHand("3D 6D 7D TD QD"))
    assert(PokerHand("4D 6S 9H QH QC") > PokerHand("3D 6D 7H QD QS"))
    assert(PokerHand("2H 2D 4C 4D 4S") > PokerHand("3C 3D 3S 9S 9D"))
  }
}
