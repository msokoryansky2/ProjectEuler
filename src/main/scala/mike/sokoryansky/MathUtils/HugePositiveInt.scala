package mike.sokoryansky.MathUtils

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
    * Find square root of a HugePositiveInt as non-rounded HugePositiveInt. E.g.
    * HugePositiveInt(122).sqrt === 11, HugePositiveInt(143).sqrt === 11 (of course, HugePositiveInt(144).sqrt === 12).
    * Square root is computed using binary-ish search for each digit
    */
  def sqrt: HugePositiveInt = {
    // Number of digits in the answer is always ceiling of number of digits in the original / 2.
    // E.g. square roots of all 7- or 8-digit numbers are always 4 digits long.
    val digits = (value.length + 1) / 2
    def sqrtAcc(d: Long, lastGuess: Int, guessHistory: List[Int], acc: String): HugePositiveInt = {
      if (d > digits) HugePositiveInt(acc)
      else {
        // Create full last guess number
        val lastGuessValue = HugePositiveInt(acc + lastGuess.toString + "0" * (digits - acc.length - 1))
        val lastGuessSqr = lastGuessValue ^ 2
        lastGuessSqr.compare(this) match {
          case 0 =>
            // Exact match, we found our sqrt
            lastGuessValue
          case over if over > 0 =>
            // Our sqrt is too big, will need to try with a lower guess -- but higher than next lowest existing guess
            val highestLowerGuess = guessHistory.find(m => m < lastGuess && !guessHistory.exists(n => n > m && n < lastGuess)).getOrElse(0)
            // if our highestLowerGuess is just 1 less than last guess, then that guess is the current digit and we move on
            if (lastGuess - highestLowerGuess <= 1) sqrtAcc(d + 1, 5, List(), acc + highestLowerGuess)
            else sqrtAcc(d, highestLowerGuess + (lastGuess - highestLowerGuess) / 2, lastGuess :: guessHistory, acc)
          case under if under < 0 =>
            // Our sqrt is too small, will need to try with a higher guess -- but lower than next highest existing guess
            val lowestHigherGuess = guessHistory.find(m => m > lastGuess && !guessHistory.exists(n => n < m && n > lastGuess)).getOrElse(10)
            // if our highestLowerGuess is just 1 less than last guess, then our last guess is the current digit and we move on
            if (lowestHigherGuess - lastGuess <= 1) sqrtAcc(d + 1, 5, List(), acc + lastGuess)
            else sqrtAcc(d, lastGuess + (lowestHigherGuess - lastGuess) / 2, lastGuess :: guessHistory, acc)
        }
      }
    }
    sqrtAcc(1, 5, List(), "")
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

  /**
    * Return all powers of number that have the same number of digits as the power
    */
  def powersSameAsNumDigits: List[HugePositiveInt] = {
    def powersSameAsNumDigitsAcc(lastNumber: HugePositiveInt,
                                 lastPower: Long,
                                 acc: List[HugePositiveInt]): List[HugePositiveInt] = {
      val nextPower = lastPower + 1
      val nextNumber = lastNumber * this
      if (nextNumber.toString.length != nextPower) acc
      else powersSameAsNumDigitsAcc(nextNumber, nextPower, nextNumber :: acc)
    }
    powersSameAsNumDigitsAcc(HugePositiveInt(1), 0, List())
  }
}

object HugePositiveInt {
  def apply(stringNumber: String) = new HugePositiveInt(stringNumber)
  def apply(int: Int) = new HugePositiveInt(int.toString)
  def apply(huge: HugePositiveInt) = new HugePositiveInt(huge.value)
}