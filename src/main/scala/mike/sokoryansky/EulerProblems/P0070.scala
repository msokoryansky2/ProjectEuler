package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Prime, Integer}

/*

Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of
positive numbers less than or equal to n which are relatively prime to n.
For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

 */

class P0070 extends EulerProblem {
  def run: String = Prime.totient1toN(9999999)
      .filterNot(_._1 == 1)
      .filter(ntn => Integer.isPermutation(ntn._1, ntn._2))
      .map(ntn => ntn._1 -> ntn._1.toDouble / ntn._2)
      .minBy(_._2)._1.toString
}

object P0070 extends App {
  (new P0070).printAnswer()
}
