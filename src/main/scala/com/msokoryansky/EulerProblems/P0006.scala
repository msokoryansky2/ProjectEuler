package com.msokoryansky.EulerProblems

class P0006 extends EulerProblem {
  def sqr(x: BigInt): BigInt = x * x

  def run: String = (sqr((1 to 100).sum) - (1 to 100).map((x) => x * x).sum).toString
}

object P0006 extends App {
  (new P0006).printAnswer()
}