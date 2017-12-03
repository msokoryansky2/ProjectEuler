package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Partition

class P0078 extends EulerProblem {
  def run: String = Partition.partitionsModuloStream(1000000).dropWhile(_._2 > 0).head._1.toString
}

object P0078 extends App {
  (new P0078).printAnswer()
}