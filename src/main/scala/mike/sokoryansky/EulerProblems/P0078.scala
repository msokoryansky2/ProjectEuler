package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{SumsOfParts, Integer}

/*

Let p(n) represent the number of different ways in which n coins can be separated into piles.
For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.

OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O

Find the least value of n for which p(n) is divisible by one million.

 */

class P0078 extends EulerProblem {
  def run: String = Integer.ints(1).dropWhile(i => (SumsOfParts.count2(i) + 1) % 1000 != 0).head.toString
}

object P0078 extends App {
  (new P0078).printAnswer()
}