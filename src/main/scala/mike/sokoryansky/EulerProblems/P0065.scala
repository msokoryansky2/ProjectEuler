package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.CF

/*
... the sequence of the first ten convergents for √2 are:
1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...

What is most surprising is that the important mathematical constant,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].

The first ten terms in the sequence of convergents for e are:
2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...

The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
 */

class P0065 extends EulerProblem {
  // We index starting from 0, so 100th convergent is indexed at 99
  def run: String = CF.e(99).toFraction(99).num.toString.toList.map(_.asDigit).sum.toString
}

object P0065 extends App {
  (new P0065).printAnswer()
}