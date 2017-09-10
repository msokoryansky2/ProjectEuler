package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.{Integer, Fraction}

/*

Consider the fraction, n/d, where n and d are positive integers.
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size,
find the numerator of the fraction immediately to the left of 3/7.

 */

class P0071 extends EulerProblem {
  /**
    * Immediate thought was that the eventual answer should be something like 300,000/700,001. In other words,
    * 3/7 * (m/m) where m is largest factor of 7 < 1,000,000 and then either increasing its numerator or decreasing
    * its numerator so the final fraction is: (3 * m - 1) / (7 * m) OR (3 * m) / (7 * m + 1)
    *
    * Increasing denom actually produces a larger fraction, so I tried that, and it said it was wrong. Then I tried
    * decreasing numerator and that resulted in correct answer: 428570! Hurray!?
    *
    * I started figuring out why that was the answer and not increasing denom: 428571/1000000. Eventually I decided to
    * look at archives where I discovered that my "answer" was completely accidentally correct. I also stumbled onto
    * Farey Series mention there which leads to correct implementation below
    */
  def run: String = {
    val limit = 1000000
    def fareyWalk(f: (Long, Long)): (Long, Long) = {
      if (7 + f._2 > limit) f
      else {
        val gcd = Integer.gcd(3 + f._1, 7 + f._2)
        fareyWalk(((3 + f._1) / gcd, (7 + f._2) / gcd))
      }
    }
    fareyWalk((2, 5))._1.toString
  }
}

object P0071 extends App {
  (new P0071).printAnswer()
}
