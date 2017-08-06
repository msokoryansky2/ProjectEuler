package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class HugeInt(stringNumber: String) extends Ordered[HugeInt] {
  private val onlyDigits = stringNumber.replaceAll("^0+", "").replaceAll("[^0-9]", "")
  val hugeInt: String = if (onlyDigits.isEmpty) "0" else onlyDigits

  def numberOfDigits: Int =
    hugeInt.length

  def digit(i: Int): Option[Int] =
    if (i < 1 || i > numberOfDigits || numberOfDigits == 0) None
    else Some(hugeInt.substring(numberOfDigits - i, numberOfDigits - i + 1).toInt)

  def compare(other: HugeInt): Int =
    if (this.numberOfDigits > other.numberOfDigits) 1
    else if (this.numberOfDigits < other.numberOfDigits) -1
    else {
      if (hugeInt > other.hugeInt) 1
      else if (hugeInt < other.hugeInt) -1
      else 0
    }

  def +(other: HugeInt): HugeInt = {
    @tailrec def plusAcc(i: Int, carry: Int, acc: String): HugeInt = {
      (digit(i), other.digit(i)) match {
        case (None, None) =>
          new HugeInt(if (carry > 0) carry.toString + acc else acc)
        case (Some(c), None) =>
          val nextCarry = if (c + carry >= 10) 1 else 0
          plusAcc(i + 1, nextCarry, (c + carry - 10 * nextCarry).toString + acc)
        case (None, Some(c)) =>
          val nextCarry = if (c + carry >= 10) 1 else 0
          plusAcc(i + 1, nextCarry, (c + carry - 10 * nextCarry).toString + acc)
        case (Some(c), Some(d)) =>
          val nextCarry = if (c + d + carry >= 10) 1 else 0
          plusAcc(i + 1, nextCarry, (c + d + carry - 10 * nextCarry).toString + acc)
      }
    }
    plusAcc(1, 0, "")
  }

  def *(other: HugeInt): HugeInt = {
    @tailrec def multAcc(i: Int, acc: List[HugeInt]): HugeInt = {
      other.digit(i) match {
        case Some(digit) if digit > 0 =>
          val multDigit = (1 to digit).map((d) => this).foldLeft(new HugeInt("0"))(_ + _)
          multAcc(i + 1, new HugeInt(multDigit.hugeInt + ("0" * (i - 1))) :: acc)
        case Some(digit) if digit == 0 =>
          multAcc(i + 1, acc)
        case _ => acc.foldLeft(new HugeInt("0"))(_ + _)
      }
    }
    multAcc(1, List[HugeInt]())
  }

  def factorial: HugeInt = {
    @tailrec def factorialAcc(next: HugeInt, acc: HugeInt): HugeInt = {
      if (next >= this) acc
      else factorialAcc(next + new HugeInt("1"), acc * (next + new HugeInt("1")))
    }
    factorialAcc(new HugeInt("1"), new HugeInt("1"))
  }
}