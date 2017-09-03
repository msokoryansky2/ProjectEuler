package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Prime

/*

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*/

class P0010 extends EulerProblem {
  def run: String = Prime.primeNumberSum(2000000).toString
}

object P0010 extends App {
  (new P0010).printAnswer()
}