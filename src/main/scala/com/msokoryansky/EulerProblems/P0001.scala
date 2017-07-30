package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integer
import scala.annotation.tailrec


/*

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*/


class P0001 extends EulerProblem {
  def run: String = Integer.intsSum(0, 1000, (n) => n % 3 == 0 || n % 5 == 0).toString

  /**
    * One-liner alternative to run
    * @return String answer
    */
  def run2: String = (1 to 999).filter{n: Int => n % 3 == 0 || n % 5 == 0}.sum.toString
}

object P0001 extends App {
  (new P0001).printAnswer()
}