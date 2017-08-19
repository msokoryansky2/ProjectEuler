package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Subset

class P0053 extends EulerProblem {
  def run: String = (for {
    n <- 1 to 100
    r <- 1 until n
    c = Subset.waysToSelectRFromN(n, r)
    if c > 1000000
  } yield (n, r)).size.toString
}

object P0053 extends App {
  (new P0053).printAnswer()
}
