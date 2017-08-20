package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.HugePositiveInt

/*

A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large:
one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?

 */

class P0056 extends EulerProblem {
  def run: String = (for {
    a <- 1 until 100
    b <- 1 until 100
    a2b = BigInt(a).pow(b)
    a2bDigitSum = a2b.toString.toList.map(_.asDigit).sum
  } yield a2bDigitSum).max.toString
}

object P0056 extends App {
  (new P0056).printAnswer()
}
