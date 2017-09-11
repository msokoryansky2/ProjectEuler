package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Prime

/*

Consider the fraction, n/d, where n and d are positive integers.
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

 */

class P0072 extends EulerProblem {
  /**
    * This trivially reduces to finding sum of totients for 2 to 1,000,000. Reason is that every integer N
    * has totient(N) reduced proper fractions where N is denominator and an integer less than N is numerator.
    * This falls out from the fact that we start with N - 1 such fractions but all those where num and denom
    * have a common factor > 1 get reduced to another fraction where the numerator and denominator are both
    * divided by that factor, can no longer be reduced, and are therefore a reduced proper fraction of some number
    * less than N.
    *
    * Because our function bulk totient function starts with N == 1, we subtract 1 at the end
    * (which is the totient of 1 by definition but there are no proper fractions with denom of 1).
    */
  def run: String = (Prime.totient1toN(1000000).values.sum - 1).toString
}

object P0072 extends App {
  (new P0072).printAnswer()
}
