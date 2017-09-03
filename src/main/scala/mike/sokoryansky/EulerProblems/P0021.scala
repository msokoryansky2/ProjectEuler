package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Integer

/*
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
*/

class P0021 extends EulerProblem {
  def run: String = (for {
    i <- 1 to 10000
    j = Integer.divisors(i).filter(_ != i).sum.toInt
    if j > i && j <= 10000 && Integer.divisors(j).filter(_ != j).sum == i
  } yield i + j).sum.toString
}

object P0021 extends App {
  (new P0021).printAnswer()
}

