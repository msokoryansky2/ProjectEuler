package mike.sokoryansky.MathUtils

import scala.annotation.tailrec

object Champernowne {
  /**
    * Returns starting place of N-digit numbers in Champernowne series 0.12345678910111213...
    * Champernowne series after the decimal consists of 9 1-digit numbers, then 90 2-digit, then 900 3-digit,
    * then 9000 4-digit, etc. So 1-digit numbers start in place 1, 2-digit numbers start in place 1 + 9 * 1 = 10,
    * 3-digit in 10 + 90 * 2 = 190, 4-digit in 190 + 900 * 3 = 2890, N-digit numbers in:
    * 1 + (9e0 * 1) + (9e1 * 2) + (9e2 * 3) + (9e3 * 4) + ... + (9e(N-1) * (N-1)) =
    * 1 + 9 * (1 + 20 + 300 + 4000 + ... + e(N-1) * (N-1))
    */
  def startOfNDigitNumbers(n: Int): Long = {
    require(n > 0, "Must specify positive number of digits")
    1 + 9 * (2 to n).map(i => Math.pow(10, i - 2).toLong * (i - 1)).sum
  }

  /**
    * Returns into what range of digit numbers given nth digit of Champernowne falls into. I.e. if n is 20, 20th
    * Champernowne digit falls into range of 2-digit numbers, so return value is 2.
    */
  def getNDigitNumber(n: Long): Int = {
    require(n > 0, "Must specify positive digit number")
    @tailrec def getNDigitNumberAcc(guess: Int): Int = {
      val startOfGuessDigitNumbers = startOfNDigitNumbers(guess)
      val startOfGuessP1DigitNumbers = startOfNDigitNumbers(guess + 1)
      if (startOfGuessDigitNumbers <= n && startOfGuessP1DigitNumbers > n) guess
      else if (startOfGuessDigitNumbers < n) getNDigitNumberAcc(guess + 1)
      else getNDigitNumberAcc(guess - 1)
    }
    getNDigitNumberAcc(Math.max(1, Math.log10(n).toInt))
  }

  /**
    * Returns nth digit after the decimal in Champernowne.
    * Very, very roughly each additional digit in numbers making up expanded Champernowne increases the number
    * of digits tenfold. So we approximate the digit-range we're in with
    */
  def digit(n: Long): Int = {
    require(n > 0, "Must specify positive digit number")
    val digitLengthRange = getNDigitNumber(n)                           // I.e. n is within 4-digit number range
    val digitWithinRange = n - startOfNDigitNumbers(digitLengthRange)   // I.e. how many digits into this 4-digit range
    val numberWithinRange = digitWithinRange / digitLengthRange + 1     // I.e. what number within 4-digit range
    val digitWithinNumber = digitWithinRange.toInt % digitLengthRange   // I.e. what number digit within the number
    /*
    At this point numberWithinRange might be 5327 which means 5327th 4-digit number (meaning 6326) and
    digitWithinNumber might be 3 meaning 3rd digit from left of 6326. We construct formula to extract that "3"
     */
    val currentNumber = Math.pow(10, digitLengthRange - 1).toLong - 1 + numberWithinRange
    currentNumber.toString.charAt(digitWithinNumber.toInt).asDigit
  }
}
