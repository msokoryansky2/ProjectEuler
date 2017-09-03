package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Integer

/*

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

 */

class P0034 extends EulerProblem {
  /*
  We know we can not test numbers above ~3,000,000 because for a 7-digit number, max sum of digits' factorials is
  7 * 9! which is under 3,000,000. It only gets worse above that.
   */
  def run: String = (3 to 3000000).filter(Integer.isSumDigitFactorials).sum.toString
}

object P0034 extends App {
  (new P0034).printAnswer()
}
