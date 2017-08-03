package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.HugeInt


/*
215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 21000?
*/

class P0016 extends EulerProblem {
  def run: String = List.fill(1000)(new HugeInt("2")).foldLeft(new HugeInt("1"))(_ * _).hugeInt
    .split("").map(_.toInt).sum.toString
}

object P0016 extends App {
  (new P0016).printAnswer()
}
