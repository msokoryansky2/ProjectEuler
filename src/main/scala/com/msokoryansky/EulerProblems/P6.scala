package com.msokoryansky.EulerProblems

class P6 extends EulerProblem {
  def sqr(x: BigInt): BigInt = x * x

  def run: String = (sqr((1 to 100).sum) - (1 to 100).map((x) => x * x).sum).toString
}

object P6 extends App {
  (new P6).printAnswer()
}