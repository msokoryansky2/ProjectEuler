package com.msokoryansky.EulerProblems

import scala.annotation.tailrec

class P1 extends EulerProblem {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints(i: Int): Stream[Int] = i #:: ints(i + 1)

  /**
    * @param i first integer to consider
    * @param limit cutoff at or above which we stop
    * @param include function to decide which integers should be included in the sum
    * @return Sum of all integers starting from i and less than limit that meet the include condition
    */
  def intsSum(i: Int, limit: Int, include: Int => Boolean): Int = {
    @tailrec def intsSumAcc(ints: Stream[Int], sum: Int): Int = {
      if (ints.head >= limit) sum
      else intsSumAcc(ints.tail, sum + (if (include(ints.head)) ints.head else 0))
    }
    intsSumAcc(ints(i), 0)
  }

  def run: String = intsSum(0, 1000, (n) => n % 3 == 0 || n % 5 == 0).toString

  /**
    * One-liner alternative to run
    * @return String answer
    */
  def run2: String = (1 to 999).filter{n: Int => n % 3 == 0 || n % 5 == 0}.sum.toString
}

object P1 extends App {
  (new P1).printAnswer()
}