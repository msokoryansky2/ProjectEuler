package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.HugeInt

class P0016 extends EulerProblem {
  def run: String = List.fill(1000)(new HugeInt("2")).foldLeft(new HugeInt("1"))(_ * _).hugeInt
    .split("").map(_.toInt).sum.toString
}

object P0016 extends App {
  (new P0016).printAnswer()
}
