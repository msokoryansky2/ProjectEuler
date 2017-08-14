package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Champernowne

/*

An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

 */

class P0040 extends EulerProblem {
  def run: String = (Champernowne.digit(1) * Champernowne.digit(10) * Champernowne.digit(100) *
    Champernowne.digit(1000) * Champernowne.digit(10000) * Champernowne.digit(100000) * Champernowne.digit(1000000))
    .toString
}

object P0040 extends App {
  (new P0040).printAnswer()
}