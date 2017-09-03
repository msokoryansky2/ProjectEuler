package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Integer

/*

The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)

 */

class P0036 extends EulerProblem {
  def run: String = (1 until 1000000)
                      .filter(i => i.toString == i.toString.reverse && Integer.base2(i) == Integer.base2(i).reverse)
                        .sum.toString
}

object P0036 extends App {
  (new P0036).printAnswer()
}