package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class BiggieInt(stringNumber: String) {
  private val onlyDigits = stringNumber.replaceAll("^0+", "").replaceAll("[^0-9]", "")
  val biggieInt: String = if (onlyDigits.isEmpty) "0" else onlyDigits

  def numberOfDigits: Int =
    biggieInt.length

  def digit(i: Int): Option[Int] =
    if (i < 1 || i > numberOfDigits || numberOfDigits == 0) None
    else Some(biggieInt.substring(numberOfDigits - i, numberOfDigits - i + 1).toInt)

  def +(other: BiggieInt): BiggieInt = {
    @tailrec def plusAcc(i: Int, carry: Int, acc: String): String = {
      val blah = (digit(i), other.digit(i))
      blah match {
        case (None, None) =>
          if (carry > 0) carry.toString + acc else acc
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
    new BiggieInt(plusAcc(1, 0, ""))
  }
}