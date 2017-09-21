package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Prime, Integer, SumsOfParts}

/*

It is possible to write ten as the sum of primes in exactly five different ways:

7 + 3
5 + 5
5 + 3 + 2
3 + 3 + 2 + 2
2 + 2 + 2 + 2 + 2

What is the first value which can be written as the sum of primes in over five thousand different ways?

 */

class P0077 extends EulerProblem {
  def run: String = {
    // This is a tiny bit of cheating to just grab 1000 of the first primes instead grabbing new ones on demand,
    // but the problem statement makes it too easy. 1000 primes is clearly more than an enough. The rest is trivial.
    val primes = Prime.primes(1).take(1000).toList.map(_.toInt)
    Integer.ints(1).dropWhile(i => SumsOfParts.count(i, primes) <= 5000).head.toString
  }
}

object P0077 extends App {
  (new P0077).printAnswer()
}
