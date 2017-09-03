package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Pandigital, Prime}

/*

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

 */

class P0041 extends EulerProblem {
  def run: String = (1 to 9).flatMap(d => Pandigital.allPandigitalsMToN(1, d).filter(Prime.isPrime)).max.toString
}

object P0041 extends App {
  (new P0041).printAnswer()
}
