package com.msokoryansky.MathUtils

object HexagonalNumber {
  /**
    * Returns nth hexagonal number given by formula pn = n(2n−1)
    */
  def hexagonalNumber(n: Int): Long = {
    require(n > 0, "Must specify positive index for pentagonal number")
    n.toLong * (2 * n - 1)
  }

  /**
    * Returns whether specified number is a hexagonal one and its number in hexagonal series.
    * We derive this by solving for n given xn where
    * hn = n(2n−1):
    * 2* n*n - n = hn
    * 2*n*n - n - hn = 0
    * Using quadratic formula and only looking at positive solutions we get:
    * n = (1 + SQRT(1 + 8hn) / 4)
    * If n is an integer, we know this number is a hexagonal one
    */
  def getHexagonalNumberIndex(hn: Long): Option[Int] = {
    if (hn < 1) None
    else {
      val sqrt = Math.sqrt(1 + 8 * hn)
      if (!sqrt.isWhole() || (1 + sqrt.toInt) % 4 != 0) None
      else Some((1 + sqrt.toInt) / 4)
    }
  }

  /**
    * Checks is specified number is a hexagonal one
    */
  def isHexagonalNumber(hn: Long): Boolean = getHexagonalNumberIndex(hn).getOrElse(0) > 0

  def hexagonalNumbers(n: Int = 1): Stream[Long] = hexagonalNumber(n) #:: hexagonalNumbers(n + 1)
}
