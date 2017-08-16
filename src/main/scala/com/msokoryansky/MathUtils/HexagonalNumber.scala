package com.msokoryansky.MathUtils

object HexagonalNumber {
  /**
    * Returns nth hexagonal number given by formula pn = n(2n−1)
    */
  def hexagonalNumber(n: Int): Long = {
    require(n > 0, "Must specify positive index for pentagonal number")
    n * (2 * n - 1)
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
    if (1 * 8 * hn < 0) None
    else {
      val sqrt = Math.sqrt(1 + 8 * hn).round.toInt
      // square root term must be exact sqrt of hn and odd to make (-1 + sqrt) be divisible by 2
      if (sqrt * sqrt != (1 + 8 * hn) || (1 + sqrt) % 4 != 0) None
      else Some((1 + sqrt) / 4)
    }
  }

  /**
    * Checks is specified number is a hexagonal one
    */
  def isHexagonalNumber(hn: Long): Boolean = getHexagonalNumberIndex(hn).getOrElse(0) > 0
}
