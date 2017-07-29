package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integers

/*

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/

class P0009 extends EulerProblem {
  def run: String = Integers.pythagoreanProduct(Integers.pythagoreanTripletBySum(1000))
}

object P0009 extends App {
  (new P0009).printAnswer()
}