package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.String

import scala.io.Source

class P0079 extends EulerProblem {
  def run: String = String.shortestWithScatterSequences(Source.fromResource("p079_keylog.txt").getLines.toList)
}

object P0079 extends App {
  (new P0079).printAnswer()
}