package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class HugePositiveInt(stringNumber: String) extends Ordered[HugePositiveInt] {
  private val onlyDigits = stringNumber.replaceAll("^0+", "").replaceAll("[^0-9]", "")
  val hugePositiveInt: String = if (onlyDigits.isEmpty) "0" else onlyDigits

  def numberOfDigits: Int =
    hugePositiveInt.length

  def digit(i: Int): Option[Int] =
    if (i < 1 || i > numberOfDigits || numberOfDigits == 0) None
    else Some(hugePositiveInt.substring(numberOfDigits - i, numberOfDigits - i + 1).toInt)

  def compare(other: HugePositiveInt): Int =
    if (this.numberOfDigits > other.numberOfDigits) 1
    else if (this.numberOfDigits < other.numberOfDigits) -1
    else {
      if (hugePositiveInt > other.hugePositiveInt) 1
      else if (hugePositiveInt < other.hugePositiveInt) -1
      else 0
    }

  def +(other: HugePositiveInt): HugePositiveInt = {
    @tailrec def plusAcc(i: Int, carry: Int, acc: String): HugePositiveInt = {
      (digit(i), other.digit(i)) match {
        case (None, None) =>
          new HugePositiveInt(if (carry > 0) carry.toString + acc else acc)
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

  def *(other: HugePositiveInt): HugePositiveInt = {
    @tailrec def multAcc(i: Int, acc: List[HugePositiveInt]): HugePositiveInt = {
      other.digit(i) match {
        case Some(digit) if digit > 0 =>
          val multDigit = (1 to digit).map((d) => this).foldLeft(new HugePositiveInt("0"))(_ + _)
          multAcc(i + 1, new HugePositiveInt(multDigit.hugePositiveInt + ("0" * (i - 1))) :: acc)
        case Some(digit) if digit == 0 =>
          multAcc(i + 1, acc)
        case _ => acc.foldLeft(new HugePositiveInt("0"))(_ + _)
      }
    }
    multAcc(1, List[HugePositiveInt]())
  }

  def factorial: HugePositiveInt = {
    @tailrec def factorialAcc(next: HugePositiveInt, acc: HugePositiveInt): HugePositiveInt = {
      if (next >= this) acc
      else factorialAcc(next + new HugePositiveInt("1"), acc * (next + new HugePositiveInt("1")))
    }
    factorialAcc(new HugePositiveInt("1"), new HugePositiveInt("1"))
  }
}