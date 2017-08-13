package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.{Integer, Prime}

/*

The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?

 */

class P0035 extends EulerProblem {
  def run: String = (0 to 999999).filterNot(i => Integer.circulars(i).exists(ic => !Prime.isPrime(ic))).size.toString
}

object P0035 extends App {
  (new P0035).printAnswer()
}
