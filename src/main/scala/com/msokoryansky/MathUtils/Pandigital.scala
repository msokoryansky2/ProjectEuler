package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Pandigital {
  def pandigital1To9(i: Long): Boolean = combinedPandigital1To9(List(i))
  def combinedPandigital1To9(l: List[Long]): Boolean = l.mkString.map(_.asDigit).sortWith(_ < _) == (1 to 9).toList

  /**
    * Returns true if a number is 1-9 pandigital as a product with its two multipliers.
    * E.g. 39 × 186 = 7254 and 39, 186, 7254 together use each of 1-9 digits exactly once.
    */
  def multiMultiProductPandigital1To9(i: Long): Boolean = {
    if (i.toString.toList.distinct != i.toString.toList) false
    else Integer.divisors(i).filter(d => d != 1 && d != i).exists(d => combinedPandigital1To9(List(i, d, i / d)))
  }

  def concatenatedMultiplePandigital1To9(i: Long, r: Range): Boolean = {
    val string = r.map(re => (re * i).toString).mkString
    if (string.length != 9) false else pandigital1To9(string.toLong)
  }

  def concatenatedMultiplePandigital1To9RangeFinder(i: Long): Option[Int] = {
    @tailrec def concatenatedMultiplePandigital1To9RangeFinderAcc(n: Int): Option[Int] = {
      if (n > 9) None
      else if (concatenatedMultiplePandigital1To9(i, 1 to n)) Some(n)
      else concatenatedMultiplePandigital1To9RangeFinderAcc(n + 1)
    }
    concatenatedMultiplePandigital1To9RangeFinderAcc(2)
  }

  def getConcatenatedMultiplePandigital1To9(i: Long): Option[Long] = {
    concatenatedMultiplePandigital1To9RangeFinder(i) match {
      case Some(n) => Some((1 to n).map(re => (i * re).toString).mkString.toLong)
      case None => None
    }
  }

  /**
    * All possible pandigits made of digits 1 to n
    */
  def allPandigitals1ToN(n: Int): List[Long] = {
    require(n > 0, "Must specify positive number of digits in a pandigital set")
    Permutation.permutations((1 to n).map(_.toChar).toSet).map(_.toLong)
  }
}
