package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Pythagorean

/*

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/

class P0009 extends EulerProblem {
  def run: String = {
    val tri = Pythagorean.pythagoreanTriple(1000).head
    (tri._1 * tri._2 * tri._3).toString
  }
}

object P0009 extends App {
  (new P0009).printAnswer()
}