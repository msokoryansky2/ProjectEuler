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

  test("onePair") {
    assert(Hand("5H 5C 6S 7S KD").onePair === Ranking(Rank.OnePair, (Hand("5H 5C").cards, Hand("6S 7S KD").cards)))
    assert(Hand("5H 2C 6S 7S KD").onePair === Ranking(Rank.OnePair, (Array(), Hand("5H 2C 6S 7S KD").cards)))
  }

  test("twoPairs") {
    assert(Hand("5H 5C 6S 6C KD").twoPairs === Ranking(Rank.TwoPairs, (Hand("6S 6C").cards, Hand("KD 5H 5C").cards)))
    assert(Hand("5H 2C 6S 7S KD").twoPairs === Ranking(Rank.TwoPairs, (Array(), Hand("5H 2C 6S 7S KD").cards)))
  }

  test("threeOfAKind") {
    assert(Hand("5H 5C 5S 6C KD").threeOfAKind === Ranking(Rank.ThreeOfAKind, (Hand("5H 5C 5S").cards, Hand("6C KD").cards)))
    assert(Hand("5H 2C 6S 7S KD").threeOfAKind === Ranking(Rank.ThreeOfAKind, (Array(), Hand("5H 2C 6S 7S KD").cards)))
  }

  test("straight") {
    assert(Hand("8H 9C TS JC QD").straight === Ranking(Rank.Straight, (Hand("8H 9C TS JC QD").cards, Array())))
    assert(Hand("5H 2C 6S 7S KD").straight === Ranking(Rank.Straight, (Array(), Hand("5H 2C 6S 7S KD").cards)))
  }

  test("flush") {
    assert(Hand("2H 5H 6H 8H KH").flush === Ranking(Rank.Flush, (Hand("2H 5H 6H 8H KH").cards, Array())))
    assert(Hand("2H 5H 6S 8H KH").flush === Ranking(Rank.Flush, (Array(), Hand("2H 5H 6S 8H KH").cards)))
  }

  test("fullHouse") {
    assert(Hand("5H 5C 6S 6C 5D").fullHouse === Ranking(Rank.FullHouse, (Hand("5H 5C 5D").cards, Hand("6S 6C").cards)))
    assert(Hand("5H 5C 6S 6C 4D").fullHouse === Ranking(Rank.FullHouse, (Array(), Hand("5H 5C 6S 6C 4D").cards)))
  }

  test("fourOfAKind") {
    assert(Hand("5H 5C 5S 6C 5D").fourOfAKind === Ranking(Rank.FourOfAKind, (Hand("5H 5C 5S 5D").cards, Hand("6C").cards)))
    assert(Hand("5H 5C 5S 6C 6D").fourOfAKind === Ranking(Rank.FourOfAKind, (Array(), Hand("5H 5C 5S 6C 6D").cards)))
  }

  test("straightFlush") {
    assert(Hand("5H 6H 7H 8H 9H").straightFlush === Ranking(Rank.StraightFlush, (Hand("5H 6H 7H 8H 9H").cards, Array())))
    assert(Hand("5H 6H 7H 8H TH").straightFlush === Ranking(Rank.StraightFlush, (Array(), Hand("5H 6H 7H 8H TH").cards)))
  }

  test("royalFlush") {
    assert(Hand("TH JH QH KH AH").royalFlush === Ranking(Rank.RoyalFlush, (Hand("TH JH QH KH AH").cards, Array())))
    assert(Hand("9H JH QH KH AH").royalFlush === Ranking(Rank.RoyalFlush, (Array(), Hand("9H JH QH KH AH").cards)))
  }

  test("highestRanking") {
    assert(Hand("3C 3D 3S 9S 9D").highestRanking === Hand("3C 3D 3S 9S 9D").fullHouse)
    assert(Hand("TH JH QH KH AH").highestRanking === Hand("TH JH QH KH AH").royalFlush)
    assert(Hand("TH JH 2S KH AH").highestRanking === Hand("TH JH 2S KH AH").highCard)
  }

  test("Can compare two poker hands") {
    assert(Hand("5H 5C 6S 7S KD") < Hand("2C 3S 8S 8D TD"))
    assert(Hand("5D 8C 9S JS AC") > Hand("2C 5C 7D 8S QH"))
    assert(Hand("2D 9C AS AH AC") < Hand("3D 6D 7D TD QD"))
    assert(Hand("4D 6S 9H QH QC") > Hand("3D 6D 7H QD QS"))
    assert(Hand("2H 2D 4C 4D 4S") > Hand("3C 3D 3S 9S 9D"))
  }
}
