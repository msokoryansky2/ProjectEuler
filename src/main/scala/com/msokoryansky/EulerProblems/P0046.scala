package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils._

/*

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

 */

class P0046 extends EulerProblem {
  def run: String = Goldbach.goldbachCandidates.find(Goldbach.goldbachFactor(_).isEmpty).get.toString

}

object P0046 extends App {
  (new P0046).printAnswer()
}
