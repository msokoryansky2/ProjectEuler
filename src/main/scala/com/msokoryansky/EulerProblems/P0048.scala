package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.HugePositiveInt

/*

The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

 */

class P0048 extends EulerProblem {
  def run: String = (1 to 1000).filterNot(_ % 10 == 0).map(HugePositiveInt(_))
    .map(i => List.fill(i.value.toInt)(i)
              .foldLeft(HugePositiveInt("1"))((j, k) => j.lastDigits(k, 10, (n1, n2) => n1 * n2)))
    .foldLeft(HugePositiveInt("0"))((j, k) => j.lastDigits(k, 10, (n1, n2) => n1 + n2))
    .value
}

object P0048 extends App {
  (new P0048).printAnswer()
}