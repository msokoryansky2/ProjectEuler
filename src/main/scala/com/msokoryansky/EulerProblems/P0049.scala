package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Prime

/*

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
(i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?

 */

class P0049 extends EulerProblem {

  def run: String = (for {
    i <- 1488 to 9999 - 2 * 3330
    if Prime.isPrime(i)
    i2 = i + 3330
    if Prime.isPrime(i2)
    if i.toString.toList.sortWith(_ < _) == i2.toString.toList.sortWith(_ < _)
    i3 = i2 + 3330
    if Prime.isPrime(i3)
    if i.toString.toList.sortWith(_ < _) == i3.toString.toList.sortWith(_ < _)
  } yield i.toString + i2.toString + i3.toString).head
}

object P0049 extends App {
  (new P0049).printAnswer()
}
