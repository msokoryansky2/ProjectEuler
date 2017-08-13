package com.msokoryansky.MathUtils

object Pandigital {
  def combinedPandigital1To9(l: List[Long]): Boolean = l.mkString.map(_.asDigit).sortWith(_ < _) == (1 to 9).toList

  /**
    * Returns true if a number is 1-9 pandigital as a product with its two multipliers.
    * E.g. 39 × 186 = 7254 and 39, 186, 7254 together use each of 1-9 digits exactly once.
    */
  def multiMultiProductPandigital1To9(i: Long): Boolean = {
    if (i.toString.toList.distinct != i.toString.toList) false
    else Integer.divisors(i).filter(d => d != 1 && d != i).exists(d => combinedPandigital1To9(List(i, d, i / d)))
  }
}
