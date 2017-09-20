package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.SumsOfParts

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
  val sum = 100
  def run: String = SumsOfParts.count2(sum).toString
}

object P0076 extends App {
  (new P0076).printAnswer()
}
