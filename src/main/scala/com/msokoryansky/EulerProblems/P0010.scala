package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integers

/*

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*/

class P0010 extends EulerProblem {
  def run: String = Integers.primeNumberSum(2000000).toString
}

object P0010 extends App {
  (new P0010).printAnswer()
}