package com.msokoryansky.EulerProblems

import com.msokoryansky.MathUtils.Pandigital

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