package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.NumberWords

class P0017 extends EulerProblem {
  def run: String = (1 to 1000).map(NumberWords.word(_)).mkString(", ").replaceAll("[^a-zA-Z]", "").length.toString
}

object P0017 extends App {
  (new P0017).printAnswer()
}
