package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Prime

import scala.annotation.tailrec

/*

The prime 41, can be written as the sum of six consecutive primes:
41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?

 */

class P0050 extends EulerProblem {
  def run: String = Prime.longestPrimeSumOfConsecutivePrimes(1000000).sum.toString
}

object P0050 extends App {
  (new P0050).printAnswer()
}
