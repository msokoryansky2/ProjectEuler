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

  def canEqual(other: Any): Boolean = other.isInstanceOf[HugePositiveInt]
  override def equals(that: Any): Boolean =
    that match {
      case that: HugePositiveInt => that.canEqual(this) && this.compare(that) == 0
      case _ => false
    }

  override def toString: String = value

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

  /**
    * Returns specified number of last digits of performing n1 op n2.
    * Operation is performed by taking only last digits of n1 and n2 as HugePositiveInt, performing op on them,
    * and again taking last digits.
    */
  def lastDigits(other: HugePositiveInt, numDigits: Int,
                 op: (HugePositiveInt, HugePositiveInt) => HugePositiveInt): HugePositiveInt = {
    require(numDigits > 0, "Must specify positive number of digits")
    HugePositiveInt(
      op(HugePositiveInt(this.value.takeRight(numDigits)), HugePositiveInt(other.value.takeRight(numDigits)))
        .value.takeRight(numDigits))
  }

  def reverse: HugePositiveInt = HugePositiveInt(value.reverse)

  def isPalindrome: Boolean = this == reverse

  def presumedLychrel(cycles: Int = 50): Boolean = {
    def checkLychrel(cycle: Int, acc: HugePositiveInt): Boolean = {
      if (cycle > cycles) true
      else {
        val next = acc + acc.reverse
        if (next.isPalindrome) false
        else checkLychrel(cycle + 1, next)
      }
    }
    checkLychrel(1, this)
  }
}

object HugePositiveInt {
  def apply(stringNumber: String) = new HugePositiveInt(stringNumber)
  def apply(int: Int) = new HugePositiveInt(int.toString)
  def apply(huge: HugePositiveInt) = new HugePositiveInt(huge.value)

  /**
    * Returns sqrt(2) expansion in form of (numerator, denominator) after specified number of expansion iterations
    * Eg:
    * sqrtOf2Expansion(1) = 1 + 1/2 = (3, 2)
    * sqrtOf2Expansion(2) = 1 + 1/(2 + 1/2) = (7, 5)
    * sqrtOf2Expansion(3) = 1 + 1/(2 + 1/(2 + 1/2)) = (17, 12)
    */
  def sqrtOf2Expansion(iters: Int): (HugePositiveInt, HugePositiveInt) = {
    def sqrtOf2Fraction(iter: Int, acc: (HugePositiveInt, HugePositiveInt)): (HugePositiveInt, HugePositiveInt) = {
      if (iter >= iters) (acc._1, acc._2)
      else {
        val expansion = sqrtOf2Fraction(iter + 1, (HugePositiveInt(1), HugePositiveInt(2)))
        (expansion._2, expansion._2 * HugePositiveInt(2) + expansion._1)
      }
    }
    val fraction = sqrtOf2Fraction(1, (HugePositiveInt(1), HugePositiveInt(2)))
    (fraction._2 + fraction._1, fraction._2)
  }
}