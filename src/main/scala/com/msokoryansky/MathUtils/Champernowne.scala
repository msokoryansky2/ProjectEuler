package com.msokoryansky.MathUtils

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
}
