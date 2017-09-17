package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Integer

/*

It is possible to write five as a sum in exactly six different ways:

4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?

 */

class P0076 extends EulerProblem {
  def run: String = Integer.sums(100).size.toString
}

object P0076 extends App {
  (new P0076).printAnswer()
}
