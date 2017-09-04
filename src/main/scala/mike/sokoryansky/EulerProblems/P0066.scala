package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Diophantine, Integer, Pell}

/*

Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

32 – 2×22 = 1
22 – 3×12 = 1
92 – 5×42 = 1
52 – 6×22 = 1
82 – 7×32 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

 */

class P0066 extends EulerProblem {
  // These are Pell Equations so we use Pell-specific implementation with fast solution
  def run: String = (1 to 1000).filterNot(d => Integer.isPow(d, 2)).map(d => d -> Pell(d).xFirst).maxBy(_._2)._1.toString
}

object P0066 extends App {
  (new P0066).printAnswer()
}