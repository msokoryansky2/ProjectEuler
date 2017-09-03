package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.HugePositiveInt


/*
215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 21000?
*/

class P0016 extends EulerProblem {
  def run: String = List.fill(1000)(new HugePositiveInt("2")).foldLeft(new HugePositiveInt("1"))(_ * _).value
    .split("").map(_.toInt).sum.toString
}

object P0016 extends App {
  (new P0016).printAnswer()
}
