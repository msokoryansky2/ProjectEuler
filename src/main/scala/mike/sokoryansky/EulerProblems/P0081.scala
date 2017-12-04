package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.NumberField

import scala.io.Source
import scala.util.Properties

/*

In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
by only moving to the right and down, is indicated in bold red and is equal to 2427.

...

Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.

 */

class P0081 extends EulerProblem {
  val field = NumberField(Source.fromResource("p081_matrix.txt").getLines.mkString(Properties.lineSeparator))
  def run: String = field.evalPath(field.bestPathMin()).toString
}

object P0081 extends App {
  (new P0081).printAnswer()
}
