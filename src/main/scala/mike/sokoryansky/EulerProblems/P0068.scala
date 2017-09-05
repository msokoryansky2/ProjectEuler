package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.NGon

class P0068 extends EulerProblem {
  def run: String = NGon.ngons((1.toLong to 6).toSet).map(_.ngonValue).filter(_.length < 16).max
}

object P0068 extends App {
  (new P0068).printAnswer()
}