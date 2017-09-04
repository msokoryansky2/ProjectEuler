package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Diophantine, Integer}

class P0066 extends EulerProblem {
  def run: String = (for {
    d <- 2 to 3
    if !Integer.isPow(d, 2)
    dio = Diophantine(1, 2, d, 2, 1)
    y = dio.yFind
    if y.nonEmpty
    x = dio.x(y.get)
    if x.nonEmpty
  } yield x).max.toString
}

object P0066 extends App {
  (new P0066).printAnswer()
}