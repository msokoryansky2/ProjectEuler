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

  /**
    * Note that this is another take on SumsOfParts.generate and one correct solution as trivial as it is
    * hopelessly slow: SumsOfParts.generate(sum, (1 until sum).toSet).size.toString
    * Because the tree of possibilities grows exponentially, enumerating all possible sums is hopeless. A good
    * solution will compute their number without enumerating them
    */
  def run: String = SumsOfParts.count(sum, 1 until sum).toString
}

object P0076 extends App {
  (new P0076).printAnswer()
}
