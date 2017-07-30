package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Prime

/*

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
*/

class P0007 extends EulerProblem {
  def run: String = Prime.primeNumber(10001).toString
}


object P0007 extends App {
  (new P0007).printAnswer()
}