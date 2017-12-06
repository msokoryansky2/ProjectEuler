package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils._

import scala.io.Source
import scala.util.Properties

/*

In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by moving left, right, up,
and down, is indicated in bold red and is equal to 2297.

...

Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."), a 31K text file containing a
80 by 80 matrix, from the top left to the bottom right by moving left, right, up, and down.

 */

class P0083 extends EulerProblem {
  val field: NumberField = NumberField(Source.fromResource("p083_matrix.txt").getLines.mkString(Properties.lineSeparator))
    .withDirs(List(NFDir.U, NFDir.R, NFDir.D, NFDir.L))
    .withValue(new NFPathSum)
    .withFitness(new NFMinPath)
    .withStart(new NFPathStartTopLeft)
    .withFinish(new NFPathFinishBottomRight)
  def run: String = field.value.eval(field.els(field.bestPath)).toString
}

object P0083 extends App {
  (new P0083).printAnswer()
}
