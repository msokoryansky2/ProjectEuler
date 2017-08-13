package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class Fraction(val num: Long, val denom: Long) extends Ordered[Fraction] {
  require(denom != 0, "Cannot have denominator of 0")

  lazy val decimal: Float = num / denom
  lazy val isWhole: Boolean = simplify.denom == 1

  override def toString: String = num.toString + "/" + denom.toString

  def compare(other: Fraction): Int = this.num * other.denom - this.denom * other.num match {
    case x if x >  0 => 1
    case x if x == 0 => 0
    case x if x <  0 => -1
  }

  def ===(other: Fraction): Boolean = this.num == other.num && this.denom == other.denom

  def simplify: Fraction = {
    // Get simple cases out of the way quickly
    if (num == 0) Fraction(0, 1)
    else if (denom < 0) Fraction(0 - num, 0 - denom).simplify
    else if (num % denom == 0) Fraction(num / denom, 1)
    else if (denom % num == 0) Fraction(1, denom / num)
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
                  this)
    }
  }
}

object Fraction {
  def apply(num: Long, denom: Long) = new Fraction(num, denom)
  def apply(num: Long) = new Fraction(num, 1)
}