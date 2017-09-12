package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{FareySeq, Fraction}

/*


Consider the fraction, n/d, where n and d are positive integers.
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?

 */

class P0073 extends EulerProblem {
  /**
    * Intuitively, 1/2 liest halfway between 0 and 1 and therefore half of reduced proper fractions should be to
    * left of it and half to the right.
    */
  def run: String =
    FareySeq.numeratorsFilter2ToN(1200, (f: Fraction) => f < Fraction(1, 2) && f > Fraction(1, 3))
      .map(_._2.size).sum.toString
}

object P0073 extends App {
  (new P0073).printAnswer()
}
