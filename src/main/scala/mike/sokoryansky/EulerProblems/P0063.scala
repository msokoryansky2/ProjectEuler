package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.HugePositiveInt

/*

The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?

 */

class P0063 extends EulerProblem {
  def run: String = (1 to 9).flatMap(HugePositiveInt(_).powersSameAsNumDigits).distinct.map(_.value).size.toString
}

object P0063 extends App {
  (new P0063).printAnswer()
}
