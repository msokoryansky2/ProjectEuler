package mike.sokoryansky.MathUtils

import scala.annotation.tailrec

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
  require(a != 0 && b != 0 && d != 0, "Polygonal number P(n) = an * (bn + c) / d must not have zeroes for a, b, or d")

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
  def numbers(n: Long): Stream[Long] = number(n) #:: numbers(n + 1)

  /**
    * Check if number is a polygonal one
    */
  def isNumber(pn: Long): Boolean = indexOf(pn).nonEmpty

  /**
    * Next polygonal number larger than pn (pn itself may or may not be a polygonal number)
    */
  def numberNext(pn: Long): Long = {
    val indexFractional = indexOfFractional(pn)
    if (indexFractional < 1) number(1)
    else number(indexFractional.floor.toLong + 1)
  }

  /**
    * Prev polygonal number smaller than pn (pn itself may or may not be a polygonal number)
    */
  def numberPrev(pn: Long): Long = {
    require(pn > number(1), "There are no polygonal numbers less than " + number(1))
    val indexFractional = indexOfFractional(pn)
    number(indexFractional.ceil.toLong - 1)
  }

  /**
    * All polygonal numbers between low (inclusive) and high (exclusive)
    */
  def numbersBetween(low: Long, high: Long): IndexedSeq[Long] = {
    val first = numberNext(low - 1)
    val last = numberPrev(high)
    (indexOf(first).get to indexOf(last).get).map(number)
  }

  /**
    * Return all polygonal numbers of certain length (by number of digits)
    */
  def numbersOfLength(digits: Int): IndexedSeq[Long] = {
    require(digits > 0, "Must specify positive number of digits")
    val low = ("9" * (digits - 1)).toLong
    val high = ("1" + "0" * digits).toLong
    numbersBetween(low, high)
  }

  /**
    * Compute index of polygonal number pn (if any) from pn itself. None if pn is not polygonal number, return None
    * General formula for index is:
    * n = (-ac + sqrt(aacc + 4abd*pn)) / 2ab
    *
    * This formula falls out of quadratic equation:
    * pn = an * (bn + c) / d
    * pn * d = ab * n * n + ac * n
    * ab * n * n + ac * n - pn * d = 0
    * and solving quadratic eq for n with a, b, c, d, pn being constants
    *
    * The index may be fractional which means it's not a polygonal number, but the fractional value is useful
    * in identifying next or previous polygonal numbers
    */
  def indexOf(pn: Long): Option[Long] = {
    val indexFractional = indexOfFractional(pn)
    if (!indexFractional.isWhole() || indexFractional < 1) None else Some(indexFractional.toLong)
  }
  def indexOfFractional(pn: Long): Double = {
    val sqrt: Double = Math.sqrt(a * a * c * c + 4 * a * b * d * pn)
    (-1 * a * c + sqrt) / (2 * a * b)
  }

  def distance(n: Long, m: Long): Long = Math.abs(PolygonalNumber(5).number(n) - PolygonalNumber(5).number(m))

  /**
    * Returns index of first polygonal number whose next neighbor is more than specified distance away.
    * Distance between polygona number indexed n and its next neighbor is:
    * (ab(n + 1)(n + 1) - abnn + ac(n + 1) - ac) / d = (2abn + ab + ac) / d
    * Setting that equal to distance and solving for n we get:
    * distance = (2abn + ab + acn) / d
    * d * distance - ab = 2abn + acn
    * n = (d * distance - ab) / (2ab + ac)
    */
  def indexOfFirstRemote(distance: Long): Long = {
    val n = (d.toDouble * distance - a * b) / (2.toDouble * a * b + a * c)
    if (n.isWhole()) n.toLong else n.toLong + 1
  }

  /**
    * Exhaustive search of polygonal series for first polygonal pair satisfying specified
    * property, beginning with index start
    */
  def indexOfFirstPairWithProperty(start: Long, p: (Long, Long) => Boolean): (Long, Long) = {
    require(start > 0, "Starting index must be positive")
    @tailrec def indexOfFirstPairWithPropertyWalk(current: Long, end: Long): (Long, Long) = {
      if (current == 0) indexOfFirstPairWithPropertyWalk(end, end + 1)
      else if (p(current, end)) (current, end)
      else indexOfFirstPairWithPropertyWalk(current - 1, end)
    }
    indexOfFirstPairWithPropertyWalk(start, start + 1)
  }

  /**
    * Exhaustive search of all pairs of pentagonals meeting specified property,
    * between start and end index, inclusive, and within specified distance
    */
  def indexOfAllPairsWithinDistanceWithProperty(start: Long,
                                                finish: Long,
                                                dist: Long,
                                                p: (Long, Long) => Boolean): List[(Long, Long)] = {
    require(start > 0, "Starting index must be positive")
    require(finish > 0, "Finishing index must be positive")
    require(finish > start, "Starting index must be less than the finishing index")
    @tailrec def indexOfAllPairsWithinDistanceWithPropertyAcc(current: Long,
                                                              end: Long,
                                                              acc: List[(Long, Long)]): List[(Long, Long)] = {
      if (current < start || distance(current, end) > dist) {
        if (end >= finish) acc
        else indexOfAllPairsWithinDistanceWithPropertyAcc(end , end + 1, acc)
      }
      else if (p(current, end)) indexOfAllPairsWithinDistanceWithPropertyAcc(current - 1, end, (current, end) :: acc)
      else indexOfAllPairsWithinDistanceWithPropertyAcc(current - 1, end, acc)
    }
    indexOfAllPairsWithinDistanceWithPropertyAcc(start, start + 1, List[(Long, Long)]())
  }

  /**
    * Checks if both sum and difference of two specified numbers are polygonal numbers
    */
  def polygonalSumAndDiff(n1: Long, n2: Long): Boolean = {
    val pn1 = number(n1)
    val pn2 = number(n2)
    isNumber(pn1 + pn2) && isNumber(Math.abs(pn1 - pn2))
  }
 }

object PolygonalNumber {
  def apply(a: Int, b: Int, c: Int, d: Int): PolygonalNumber = new PolygonalNumber(a, b, c, d)
  def apply(ness: Int): PolygonalNumber = {
    require(ness >= 3 && ness <= 8, "Must specify polygonal-ness between 3 and 8")
    ness match {
      case 3 => PolygonalNumber(1, 1, 1, 2)
      case 4 => PolygonalNumber(1, 1, 0, 1)
      case 5 => PolygonalNumber(1, 3, -1, 2)
      case 6 => PolygonalNumber(1, 2, -1, 1)
      case 7 => PolygonalNumber(1, 5, -3, 2)
      case 8 => PolygonalNumber(1, 3, -2, 1)
    }
  }
}