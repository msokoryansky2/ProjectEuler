package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Collatz

class P0014 extends EulerProblem {
  def run: String = Collatz.longestCollatz(1, 1000000).toString
}

object P0014 extends App {
  (new P0014).printAnswer()
}