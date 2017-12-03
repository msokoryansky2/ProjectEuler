package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Partition

class P0078 extends EulerProblem {
  def run: String = Partition.partitionsModuloStream(100).head._1.toString
}

object P0078 extends App {
  (new P0078).printAnswer()
}