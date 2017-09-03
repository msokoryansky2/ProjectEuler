package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.LatticePath

/*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
 */

class P0015 extends EulerProblem {
  def run: String = LatticePath.numPaths(20, 20).toString
}

object P0015 extends App {
  (new P0015).printAnswer()
}