package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integers

/*

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*/


class P0003 extends EulerProblem {
  def run: String = Integers.primeFactors(BigInt("600851475143"), Integers.primes(Integers.ints(2)), Nil).max.toString
}

object P0003 extends App {
  (new P0003).printAnswer()
}