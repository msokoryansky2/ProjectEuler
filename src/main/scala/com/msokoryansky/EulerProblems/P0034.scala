package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integer

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
