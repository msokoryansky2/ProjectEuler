package com.msokoryansky.MathUtils

class Hand private(val cards: Array[Card]) extends Ordered[Hand] {
  // Note that we allow PokerHand with less than 5 cards to resolve comparison conflicts
  // by removing equally-valued "best" cards from both hands

  override def toString: String = cards.map(_.toString).mkString(" ")

  lazy val highCard: Ranking =
    Ranking(Rank.High, cards.partition(c => c == cards.maxBy(_.value)))

  lazy val onePair: Ranking =
    Ranking(Rank.OnePair, cards.partition(c => cards.count(_.value == c.value) == 2))

  lazy val twoPairs: Ranking =
    Ranking(Rank.TwoPairs, cards.partition(c => cards.count(_.value == c.value) == 2 &&
      cards.exists(d => cards.count(_.value == d.value) == 2 && c != d && c.value.id > d.value.id)))

  lazy val threeOfAKind: Ranking =
    Ranking(Rank.ThreeOfAKind, cards.partition(c => cards.count(_.value == c.value) == 3))

  lazy val straight: Ranking = {
    Ranking(Rank.Straight,
      cards.map(c => c.value.id).sortWith(_ < _).sliding(2).filter(cc => cc.tail.head - cc.head != 1) match {
        case e if e.isEmpty => (cards, Array())
        case _ => (Array(), cards)
      }
    )
  }

  lazy val flush: Ranking =
    Ranking(Rank.Flush, cards.partition(c => !cards.exists(_.suit != c.suit)))

  lazy val fullHouse: Ranking =
    if (threeOfAKind.exists && Hand(threeOfAKind.tiebreaker).onePair.exists) Ranking(Rank.FullHouse, threeOfAKind.cards)
    else Ranking(Rank.FullHouse, (Array(), cards))

  lazy val fourOfAKind: Ranking =
    Ranking(Rank.FourOfAKind, cards.partition(c => cards.count(_.value == c.value) == 4))

  lazy val straightFlush: Ranking =
    if (straight.exists && flush.exists) Ranking(Rank.StraightFlush, straight.cards)
    else Ranking(Rank.StraightFlush, (Array(), cards))

  lazy val royalFlush: Ranking =
    if (straightFlush.exists && cards.exists(_.value == CardValue.Ace)) Ranking(Rank.RoyalFlush, straightFlush.cards)
    else Ranking(Rank.RoyalFlush, (Array(), cards))

  lazy val highestRanking: Ranking =
    if (royalFlush.exists) royalFlush
    else if (straightFlush.exists) straightFlush
    else if (fourOfAKind.exists) fourOfAKind
    else if (fullHouse.exists) fullHouse
    else if (flush.exists) flush
    else if (straight.exists) straight
    else if (threeOfAKind.exists) threeOfAKind
    else if (twoPairs.exists) twoPairs
    else if (onePair.exists) onePair
    else highCard

  override def compare(that: Hand): Int = {
    if (highestRanking.rank > that.highestRanking.rank) 1
    else if (highestRanking.rank < that.highestRanking.rank) -1
    else if (highestRanking.tiebreaker.isEmpty || that.highestRanking.tiebreaker.isEmpty) 0
    else Hand(highestRanking.tiebreaker) compare Hand(that.highestRanking.tiebreaker)
  }
}

object Hand {
  def apply(cards: Array[Card]): Hand = new Hand(cards)
  def apply(hand: Hand): Hand = new Hand(hand.cards)
  def apply(str: String): Hand = new Hand(str.split("\\s|,").map(c => Card.fromString(c)))
}

case class Ranking(rank: Rank.Value, cards: (Array[Card], Array[Card])) {
  def exists: Boolean = cards._1.length > 0
  def rankCards: Array[Card] = cards._1
  def tiebreaker: Array[Card] = cards._2

  override def toString: String = rank + ": [" + Hand(rankCards).toString + "], [" + Hand(tiebreaker).toString + "]"

  override def equals(obj: scala.Any): Boolean =
    obj.isInstanceOf[Ranking] &&
      rank == obj.asInstanceOf[Ranking].rank &&
      rankCards.toSet == obj.asInstanceOf[Ranking].rankCards.toSet &&
      tiebreaker.toSet == obj.asInstanceOf[Ranking].tiebreaker.toSet
}

object Rank extends Enumeration {
  type Rank = Value
  val High          = Value(2, "High")
  val OnePair       = Value(3, "OnePair")
  val TwoPairs      = Value(4, "TwoPairs")
  val ThreeOfAKind  = Value(5, "ThreeOfAKind")
  val Straight      = Value(6, "Straight")
  val Flush         = Value(7, "Flush")
  val FullHouse     = Value(8, "FullHouse")
  val FourOfAKind   = Value(9, "FourOfAKind")
  val StraightFlush = Value(10, "StraightFlush")
  val RoyalFlush    = Value(11, "RoyalFlush")
}

case class Card private (value: CardValue.Value, suit: Suit.Value) extends Ordered[Card] {
  override def toString: String = this.value.toString + this.suit.toString
  override def compare(that: Card): Int = this.value.id - that.value.id
}

object Card {
  def apply(str: String): Card = fromString(str)

  def fromString(str: String): Card = {
    require(str.length == 2, "String description of a card must have 2 characters")
    require(CardValue.values.exists(_.toString == str(0).toString), "Invalid card value")
    require(Suit.values.exists(_.toString == str(1).toString), "Invalid card suit")
    val v = CardValue.withName(str(0).toString)
    val s = Suit.withName(str(1).toString)
    new Card(v, s)
  }
}

object CardValue extends Enumeration {
  type CardValue = Value
  val Two    = Value(2, "2")
  val Three  = Value(3, "3")
  val Four   = Value(4, "4")
  val Five   = Value(5, "5")
  val Six    = Value(6, "6")
  val Seven  = Value(7, "7")
  val Eight  = Value(8, "8")
  val Nine   = Value(9, "9")
  val Ten    = Value(10, "T")
  val Jack   = Value(11, "J")
  val Queen  = Value(12, "Q")
  val King   = Value(13, "K")
  val Ace    = Value(14, "A")
}

object Suit extends Enumeration {
  type Suit = Value
  val Hearts   = Value(1, "H")
  val Diamonds = Value(2, "D")
  val Clubs    = Value(3, "C")
  val Spades   = Value(4, "S")
}