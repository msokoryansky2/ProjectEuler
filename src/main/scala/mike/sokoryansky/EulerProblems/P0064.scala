package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.CF

/*
The first ten continued fraction representations of (irrational) square roots are:

√2=[1;(2)], period=1
√3=[1;(1,2)], period=2
√5=[2;(4)], period=1
√6=[2;(2,4)], period=2
√7=[2;(1,1,1,4)], period=4
√8=[2;(1,4)], period=2
√10=[3;(6)], period=1
√11=[3;(3,6)], period=2
√12= [3;(2,6)], period=2
√13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?
 */

class P0064 extends EulerProblem {
  def run: String = (1 to 10000).map(CF.sqrt(_)).count(_.fractionRepeat.size % 2 == 1).toString
}

object P0064 extends App {
  (new P0064).printAnswer()
}
