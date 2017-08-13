package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Fraction

/*

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may
incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing
two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

 */

class P0033 extends EulerProblem {
  def run: String = (for {
    n <- 10 to 99
    d <- 10 to 99
    if n < d && (n % 10 != 0 || d % 10 != 0)
    f = Fraction(n, d)
    if f.isCurious2Digit
  } yield f).foldLeft(Fraction(1, 1))(_ *_).simplify.toString
}

object P0033 extends App {
  (new P0033).printAnswer()
}
