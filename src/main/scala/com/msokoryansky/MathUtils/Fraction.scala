package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class Fraction(val num: Long, val denom: Long) extends Ordered[Fraction] {
  require(denom != 0, "Cannot have denominator of 0")

  lazy val decimal: Float = num / denom
  lazy val isWhole: Boolean = simplify.denom == 1

  override def toString: String = num.toString + "/" + denom.toString

  def compare(that: Fraction): Int = {
    val this2 = this.simplifyPositiveDenom
    val that2 = that.simplifyPositiveDenom

    if (this2.num < 0 && that2.num >= 0) -1
    else if (this2.num > 0 && that2.num <= 0) 1
    else if (this2.num < 0 && that2.num < 0)
      -1 * Fraction(Math.abs(this2.num), this2.denom).compare(Fraction(Math.abs(that2.num), that2.denom))
    else this2.num * that2.denom - this2.denom * that2.num match {
      case x if x > 0  => 1
      case x if x == 0 => 0
      case x if x < 0  => -1
    }
  }

  override def equals(that: Any): Boolean =
    that match {
      case fraction: Fraction => compare(fraction) == 0
      case _ => super.equals(that)
    }

  def simplifyPositiveDenom: Fraction = if (denom < 0) Fraction(0 - num, 0 - denom) else this

  def simplify: Fraction = {
    // Get simple cases out of the way quickly
    if (num == 0) Fraction(0, 1)
    else if (num % denom == 0) Fraction(num / denom, 1).simplifyPositiveDenom
    else if (denom % num == 0) Fraction(1, denom / num).simplifyPositiveDenom
    else {
      @tailrec def simplifyAcc(numFactors: List[Long], denomFactors: List[Long], acc: Fraction): Fraction =
        numFactors match {
          case Nil => acc
          case firstNumFactor :: otherNumFactors =>
            if (!denomFactors.contains(firstNumFactor)) simplifyAcc(otherNumFactors, denomFactors, acc)
            else Fraction(num / firstNumFactor, denom / firstNumFactor).simplify
        }
      simplifyAcc(Integer.divisors(Math.abs(num)).filter(_ != 1).toList,
                  Integer.divisors(Math.abs(denom)).filter(_ != 1).toList,
                  this).simplifyPositiveDenom
    }
  }

  def simplifiable: Boolean = this.num != simplify.num ||  this.denom != simplify.denom

  def +(that: Fraction): Fraction =
    Fraction(this.num * that.denom + that.num * this.denom, this.denom * that.denom).simplify
  def -(that: Fraction): Fraction =
    Fraction(this.num * that.denom - that.num * this.denom, this.denom * that.denom).simplify
  def *(that: Fraction): Fraction =
    Fraction(this.num * that.num, this.denom * that.denom).simplify
  def /(that: Fraction): Fraction = {
    require(that.num != 0, "Numerator of the divisor is zero. Cannot divide by zero")
    Fraction(this.num * that.denom, this.denom * that.num).simplify
  }

  def isCurious2Digit: Boolean =
    num >= 10 && num <= 99 && denom >= 10 && denom <= 99 &&
    (denom % 10 != 0 && num / 10 == denom / 10 && this == Fraction(num % 10, denom % 10)) ||
      (denom / 10 != 0 && num / 10 == denom % 10 && this == Fraction(num % 10, denom / 10)) ||
      (denom % 10 != 0 && num % 10 == denom / 10 && this ==  Fraction(num / 10, denom % 10)) ||
      (denom / 10 != 0 && num % 10 == denom % 10 && this == Fraction(num / 10, denom / 10))
}

object Fraction {
  def apply(num: Long, denom: Long) = new Fraction(num, denom)
  def apply(num: Long) = new Fraction(num, 1)
}