package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Prime

/*

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/

class P0005 extends EulerProblem {
  def run: String = Prime.primeFactorsOfRange(1, 20).product.toString()
}

object P0005 extends App {
  (new P0005).printAnswer()
}