package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils._

import scala.io.Source
import scala.util.Properties

/*

The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and finishing in any cell
in the right column, and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.

...

Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
a 31K text file containing a 80 by 80 matrix, from the left column to the right column.

 */

class P0082 extends EulerProblem {
  val field: NumberField = NumberField(Source.fromResource("p082_mike.txt").getLines.mkString(Properties.lineSeparator))
    .withDirs(List(NFDir.D, NFDir.U, NFDir.R))
    .withValue(new NFPathSum)
    .withFitness(new NFMinPath)
    .withStart(new NFLocL)
    .withFinish(new NFLocR)
  def run: String = field.value.eval(field.els(field.bestPath)).toString
}

object P0082 extends App {
  (new P0082).printAnswer()
}
