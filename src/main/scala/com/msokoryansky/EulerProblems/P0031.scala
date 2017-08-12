package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.SumOfParts

/*

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?

 */

class P0031 extends EulerProblem {
  def run: String = SumOfParts.waysToAddUpParts(30, Set(1, 2, 5, 10, 20, 50, 100, 200)).size.toString
}

object P0031 extends App {
  (new P0031).printAnswer()
}
