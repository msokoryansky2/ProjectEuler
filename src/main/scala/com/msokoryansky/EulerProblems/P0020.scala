package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.HugeInt


/*
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
*/

class P0020 extends EulerProblem {
  def run: String = new HugeInt("100").factorial.hugeInt.split("").toList.map(_.toInt).sum.toString
}

object P0020 extends App {
  (new P0020).printAnswer()
}

