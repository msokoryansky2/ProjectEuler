package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Pandigital

/*

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

 */

class P0032 extends EulerProblem {
  /**
    * For a number to be 1-9 pandigital with its two factors it needs to be a 4-digit number.
    * 3-digit or shorter numbers will definitely be less than its potential factors that have 6 digits between them.
    * 5-digit or longer numbers will definitely be more than its potential factors that have 4 digits between them.
    */
  def run: String = (1000 to 9999).filter(Pandigital.multiMultiProductPandigital1To9(_)).sum.toString
}

object P0032 extends App {
  (new P0032).printAnswer()
}