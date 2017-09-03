package com.msokoryansky.MathUtils

import scala.collection.immutable.Queue

/**
  * Square root of a positive number can be written as continued fraction of form
  * sqrt(N) = a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 .... ))))
  *
  * A recurring element in CF expansion of square root of N is element: (sqrt(N) + numAdd) / denom
  *
  * To find a(i) of this element we compute the whole portion of the fraction, call it W, and set the entire fraction
  * equal to W + 1/a(i+1)
  *
  * For a much better expl, see http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html#section6.1
  */

case class CFSqrt(whole: Long, fractionStart: Seq[Long], fractionRepeat: Seq[Long]) {
  override def toString: String =
    new String(whole.toString
      + (if (fractionStart.nonEmpty || fractionRepeat.nonEmpty) ";" else "")
      + (if (fractionStart.nonEmpty) fractionStart.mkString(",") else "")
      + (if (fractionStart.nonEmpty && fractionRepeat.nonEmpty) "," else "")
      + (if (fractionRepeat.nonEmpty) "[" + fractionRepeat.mkString(",") + "]..." else ""))
}

object CFSqrt {
  def apply(whole: Long): CFSqrt =
    new CFSqrt(whole, List(), List())
  def apply(whole: Long, fractionStart: Seq[Long], fractionRepeat: Seq[Long]): CFSqrt =
    new CFSqrt(whole, fractionStart, fractionRepeat)

  def sqrt(number: Long): CFSqrt = {
    // The accumulator returns a queue of fractional (post-semicolon) CFSqrtElement values
    // and a Long value indicating start of repeat in the queue
    def sqrtAcc(el: CFSqrtElement, acc: Queue[CFSqrtElement]): (Queue[CFSqrtElement], Int) = {
      if (el.isNPerfectSquare) (Queue(el), -1)
      else acc.indexWhere(_ == el, 1) match {
        case start if start > 0 => (acc, start)
        case _ => sqrtAcc(el.nextElement, acc.enqueue(el))
      }
    }
    val el0 = CFSqrtElement(number, 0, 1)
    val (els, startOfRepeat) = sqrtAcc(el0, Queue[CFSqrtElement]())
    val whole = els.head.wholeValue
    val fractionStart =
      if (startOfRepeat > 0 && els.size > startOfRepeat) els.tail.take(startOfRepeat - 1).map(_.wholeValue)
      else Seq[Long]()
    val fractionRepeat =
      if (startOfRepeat > 0 && els.size > startOfRepeat) els.tail.drop(startOfRepeat - 1).map(_.wholeValue)
      else Seq[Long]()
    CFSqrt(whole, fractionStart, fractionRepeat)
  }
}

case class CFSqrtElement(numN: Long, numAdd: Long, denom: Long) {
  require(numN >= 1, "Cannot have negative square root in CF expansion")
  require(denom > 0, "Denominator in square root CF expansion must always be positive")
  require(numAdd >= 0, "Integer portion of the numerator in square root CF expansion must always be positive")
  def isNPerfectSquare: Boolean = Integer.isPow(numN, 2)
  def wholeValue: Long = (Math.sqrt(numN).floor.toLong + numAdd) / denom

  /**
    * Next element is produced as follows. We start with with current CFElement:
    *
    * (sqrt(numN) + numAdd) / denom  = wholeValue + 1 / nextWholeValue
    *
    * We need to transform it into next CFElement:
    *
    * (sqrt(numN) + nextNumAdd) / nextDenom = nextWholeValue + 1 / nextNextWholeValue
    *
    * The transform steps are:
    *
    * 1 / nextWholeValue = (sqrt(numN) + numAdd - (wholeValue * denom)) / denom
    *
    * nextWholeValue = denom / (sqrt(numN) + numAdd - (wholeValue * denom))
    *
    * Let t =  numAdd - (wholeValue * denom).
    *
    * t is always negative because (wholeValue * denom) > numAdd (but we won't try to prove it here :) Then:
    *
    * nextWholeValue = denom / (sqrt(numN) + t)
    *
    * Multiply by (sqrt(numN) - t):
    *
    * nextWholeValue = denom * (sqrt(numN) - t) / (numN - t * t)
    *
    * Simplifying (and remembering that t is negative):
    *
    * nextWholeValue = (sqrt(N) - t) / ((numN - t * t) / denom)
    *
    * ((numN - t * t) / denom) should be a positive integer (again, won't try to prove why :)
    *
    * @return next element in this CF expansion
    */
  def nextElement: CFSqrtElement = {
    val t = numAdd - wholeValue * denom
    val nextDenomFraction = Fraction(numN - t * t, denom).simplify
    assert(nextDenomFraction.denom == 1, s"Numerator of ${nextDenomFraction.toString} was not 1 as expected")
    CFSqrtElement(numN, 0 - t, nextDenomFraction.num)
  }

  override def toString: String = s"(SQRT($numN) + $numAdd) / $denom"

  override def equals(that: Any): Boolean =
    that match {
      case thatEl: CFSqrtElement =>
        thatEl.numN == this.numN && thatEl.numAdd == this.numAdd && thatEl.denom == this.denom
      case _ => super.equals(that)
    }
}

object CFSqrtElement {
  def apply(numN: Long): CFSqrtElement =
    new CFSqrtElement(numN, 0, 1)
  def apply(numN: Long, numAdd: Long, denom: Long): CFSqrtElement =
    new CFSqrtElement(numN, numAdd, denom)
}

