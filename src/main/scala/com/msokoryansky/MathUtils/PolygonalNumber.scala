package com.msokoryansky.MathUtils

/**
  * Generic class for polygonal (e.g. triangle, square, pentagonal, hexagonal, etc.) number where
  * P(n) = an * (bn + c) / d where a, b, c, and d are integer constants
  *
  * Triangle 	  	P3,n=n(n+1)/2 	  	1, 3, 6, 10, 15, ...
  * Square 	  	  P4,n=n2 	  	      1, 4, 9, 16, 25, ...
  * Pentagonal 	  P5,n=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...
  * Hexagonal 	  P6,n=n(2n−1) 	  	  1, 6, 15, 28, 45, ...
  * Heptagonal 	  P7,n=n(5n−3)/2 	  	1, 7, 18, 34, 55, ...
  * Octagonal 	  P8,n=n(3n−2) 	  	  1, 8, 21, 40, 65, ...
  */
class PolygonalNumber private (a: Int, b: Int, c: Int, d: Int) {
  require(a != 0 && b != 0 && d != 0, "Polygonal number P(n) = an * (bn + c) / d must not haze zeroes for a, b, or d")

  /**
    * Compute polygonal number at index i
    */
  def number(n: Long): Long = {
    require(n > 0, "Only positive indices are valid")
    a * n * (b * n + c ) / d
  }

  /**
    * Stream of polygonal numbers
    */
  def numbers(n: Long = 1): Stream[Long] = number(n) #:: numbers(n + 1)

  /**
    * Next polygonal number larger than pn (pn itself may or may not be a polygonal number)
    */
  def numberNext(pn: Long): Long = {
    val indexFractional = indexOfFractional(pn)
    if (indexFractional < 1) number(1)
    else number(indexFractional.floor.toLong + 1)
  }

  /**
    * Compute index of polygonal number pn (if any) from pn itself. None if pn is not polygonal number, return None
    * General formula for index is:
    * n = (-ac + sqrt(aacc - 4abd*pn)) / 2ab
    *
    * This formula falls out of quadratic equation:
    * pn = an * (bn + c) / d
    * pn * d = ab * n * n + ac * n
    * ab * n * n + ac * n - pn * d = 0
    * and solving quadratic eq for n with a, b, c, d, pn being constants
    */
  def indexOf(pn: Long): Option[Long] = {
    val indexFractional = indexOfFractional(pn)
    if (!indexFractional.isWhole() || indexFractional < 1) None else Some(indexFractional.toLong)
  }

  def indexOfFractional(pn: Long): Double = {
    val sqrt: Double = Math.sqrt(a * a * c * c - 4 * a * b * d * pn)
    (-1 * a * c + sqrt) / (2 * a * b)
  }
}

object PolygonalNumber {
  def apply(a: Int, b: Int, c: Int, d: Int): PolygonalNumber = new PolygonalNumber(a, b, c, d)
}

object P3 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 1, 1, 2)
}
object P4 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 0, 0, 1)
}
object P5 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 3, -1, 2)
}
object P6 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 2, -1, 1)
}
object P7 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 5, -3, 2)
}
object P8 {
  def apply: PolygonalNumber = PolygonalNumber.apply(1, 3, -2, 1)
}