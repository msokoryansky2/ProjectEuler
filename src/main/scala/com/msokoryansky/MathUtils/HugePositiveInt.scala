package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class HugePositiveInt(stringNumber: String) extends Ordered[HugePositiveInt] {
  private val onlyDigits = stringNumber.replaceAll("^0+", "").replaceAll("[^0-9]", "")
  val value: String = if (onlyDigits.isEmpty) "0" else onlyDigits

  def numberOfDigits: Int =
    value.length

  def digit(i: Int): Option[Int] =
    if (i < 1 || i > numberOfDigits || numberOfDigits == 0) None
    else Some(value.substring(numberOfDigits - i, numberOfDigits - i + 1).toInt)

  def compare(other: HugePositiveInt): Int =
    if (this.numberOfDigits > other.numberOfDigits) 1
    else if (this.numberOfDigits < other.numberOfDigits) -1
    else {
      if (value > other.value) 1
      else if (value < other.value) -1
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
          val multDigit = (1 to digit).map((d) => this).foldLeft(HugePositiveInt(0))(_ + _)
          multAcc(i + 1, new HugePositiveInt(multDigit.value + ("0" * (i - 1))) :: acc)
        case Some(digit) if digit == 0 =>
          multAcc(i + 1, acc)
        case _ => acc.foldLeft(new HugePositiveInt("0"))(_ + _)
      }
    }
    multAcc(1, List[HugePositiveInt]())
  }

  def ^(power: Int): HugePositiveInt = {
    require(power >= 0, "Negative powers are not supported")
    @tailrec def powerAcc(n: Int, acc: HugePositiveInt): HugePositiveInt = {
      if (n > power) acc
      else powerAcc(n + 1, this * acc)
    }
    powerAcc(1, HugePositiveInt(1))
  }

  def ^(powerFrom: Int, powerTo: Int): Map[Int, HugePositiveInt] = {
    require(powerTo >= powerFrom, "Ceiling limit for power should be higher than the floor")
    @tailrec def powerAcc(n: Int, acc: Map[Int, HugePositiveInt]): Map[Int, HugePositiveInt] = {
      if (n > powerTo) acc
      else powerAcc(n + 1, acc + (n -> (this * acc(n - 1))))
    }
    powerAcc(powerFrom + 1, Map(powerFrom -> (this ^ powerFrom)))
  }

  def factorial: HugePositiveInt = {
    @tailrec def factorialAcc(next: HugePositiveInt, acc: HugePositiveInt): HugePositiveInt = {
      if (next >= this) acc
      else factorialAcc(next + HugePositiveInt(1), acc * (next + HugePositiveInt(1)))
    }
    factorialAcc(HugePositiveInt(1), HugePositiveInt(1))
  }
}

object HugePositiveInt {
  def apply(stringNumber: String) = new HugePositiveInt(stringNumber)
  def apply(int: Int) = new HugePositiveInt(int.toString)
}