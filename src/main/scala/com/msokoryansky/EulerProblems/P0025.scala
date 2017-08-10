package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Fibonacci, HugePositiveInt}

class P0025 extends EulerProblem {
  def run: String = Fibonacci.fibsWithIndex(new HugePositiveInt("1"), new HugePositiveInt("1"), 1)
                        .filter((n) => n._1.numberOfDigits >= 1000).head._2.toString
}

object P0025 extends App {
  (new P0025).printAnswer()
}
