package com.msokoryansky.EulerProblems

class P6 extends EulerProblem {
  def sqr(x: BigInt) = x * x

  def run = (sqr((1 to 100).sum) - (1 to 100).map((x) => x * x).sum).toString
}
