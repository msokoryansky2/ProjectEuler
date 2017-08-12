package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.IntegerOps.DigitOps

/*

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 14 + 64 + 34 + 44
    8208 = 84 + 24 + 04 + 84
    9474 = 94 + 44 + 74 + 44

As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

 */

class P0030 extends EulerProblem {
  /*
  Brute force works fine here because we without doing any calculation that there is no way that a 7-digit number
  could possibly equal to the sum of its digits each raised to the power of 5. The reason is that
  the biggest such sum of digit powers is for 9999999: 7 * 9 ^ 5, which we know is less than 9 ^ 6, which we know is
  less than the smallest 7-digit number which is 1,000,000 or 10 ^ 6.

  So all we have to do is brute force on numbers between 10 and 999999.
   */
  def run: String = (10 to 999999).filter(n => n.mapDigits(_.pow(5)).sum == n).sum.toString
}

object P0030 extends App {
  (new P0030).printAnswer()
}
