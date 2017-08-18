package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Integer

/*

It can be seen that the number, 125874, and its double, 251748,
contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

 */

class P0052 extends EulerProblem {
  def run: String = Integer.ints(1.toLong).find(i => {
    val i2 = i * 2
    val i3 = i * 3
    val i4 = i * 4
    val i5 = i * 5
    val i6 = i * 6
    i.toString.toSet == i2.toString.toSet &&
    i.toString.toSet == i3.toString.toSet &&
    i.toString.toSet == i4.toString.toSet &&
    i.toString.toSet == i5.toString.toSet &&
    i.toString.toSet == i6.toString.toSet
  }).get.toString
}

object P0052 extends App {
  (new P0052).printAnswer()
}