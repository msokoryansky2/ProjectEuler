package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.LatticePath

class P0015 extends EulerProblem {
  def run: String = LatticePath.numPaths(20, 20).toString
}

object P0015 extends App {
  (new P0015).printAnswer()
}