package com.msokoryansky.MathUtils

class PokerHand private (val cards: Array[Card]) extends Ordered[PokerHand] {
  require(cards.length == 5, "A poker hand must have 5 cards")

  def highCard: Int = cards.map(c => c.value.id).max
  def onePair: Boolean = cards.exists(c => cards.count(_.value == c.value) >= 2)
  def twoPairs: Boolean = cards.exists(c => cards.count(_.value == c.value) >= 2 &&
                  cards.exists(d => cards.count(_.value == d.value) >= 2 &&
                    c != d))
  def threeOfAKind: Boolean = cards.exists(c => cards.count(_.value == c.value) >= 3)
  def straight: Boolean = {
    val consecutives = cards.map(c => c.value.id).sortWith(_ < _).sliding(2)
    !consecutives.exists(cc => cc.length != 2 || cc.tail.head - cc.head != 1)
  }
  def flush: Boolean = !cards.exists(c => cards.exists(_.suit != c.suit))
  def fullHouse: Boolean = threeOfAKind && onePair && !cards.exists(c => cards.count(_.value == c.value) == 1)
  def fourOfAKind: Boolean = cards.exists(c => cards.count(_.value == c.value) >= 4)
  def straightFlush: Boolean = straight && cards.exists(c => cards.count(_.suit == c.suit) == 5)
  def royalFlush: Boolean = straightFlush && cards.exists(_.value == CardValue.Ace)

  def value: Long = highCard +
                    100 *          (if (onePair) 1 else 0) +
                    1000 *         (if (twoPairs) 1 else 0) +
                    10000 *        (if (threeOfAKind) 1 else 0) +
                    100000 *       (if (straight) 1 else 0) +
                    1000000 *      (if (flush) 1 else 0) +
                    10000000 *     (if (fullHouse) 1 else 0) +
                    100000000 *    (if (fourOfAKind) 1 else 0) +
                    1000000000 *   (if (straightFlush) 1 else 0) +
                    10000000000L * (if (royalFlush) 1 else 0)


  override def toString: String = cards.map(_.toString).mkString(" ")
  override def compare(that: PokerHand): Int = value - that.value match {
    case neg if neg < 0 => -1
    case eq  if eq == 0 => 0
    case pos if pos > 0 => 1
  }
}

object PokerHand {
  def apply(cards: Array[Card]): PokerHand = new PokerHand(cards)
  def apply(hand: PokerHand): PokerHand = new PokerHand(hand.cards)
  def apply(str: String): PokerHand = new PokerHand(str.split("\\s|,").map(c => Card.fromString(c)))
}

case class Card private (value: CardValue.Value, suit: Suit.Value) {
  override def toString: String = this.value.toString + this.suit.toString
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