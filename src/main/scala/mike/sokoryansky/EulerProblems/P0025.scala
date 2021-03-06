package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.HugePositiveInt
import mike.sokoryansky.MathUtils.Fibonacci

/*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*/

class P0025 extends EulerProblem {
  def run: String = Fibonacci.fibsWithIndex(new HugePositiveInt("1"), new HugePositiveInt("1"), 1)
                        .filter((n) => n._1.numberOfDigits >= 1000).head._2.toString
}

object P0025 extends App {
  (new P0025).printAnswer()
}
