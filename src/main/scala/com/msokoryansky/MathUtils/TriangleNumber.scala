package com.msokoryansky.MathUtils

object TriangleNumber {
  /**
    * Returns n triangle number given by formula tn = 1/2*n*(n+1)
    */
  def triangleNumber(n: Int): Long = {
    require(n > 0, "Must specify positive index for triangle number")
    n * (n + 1) / 2
  }

  /**
    * Returns whether specified number is a triangle one and its number in triangle series.
    * We derive this by solving for n given tn where
    * tn = 1/2*n*(n+1):
    * 1/2n*n + 1/2n = tn
    * n*n + n = 2tn
    * n*n + n - 2tn = 0
    * Using quadratio formula and only looking at positive solutions we get:
    * n = (-1 + SQRT(-1 + 8tn) / 2)
    * If n is an integer, we know this number is a triangle one
    */
  def getTriangleNumberIndex(tn: Long): Option[Int] = {
    if (8 * tn <= 1) None
    else {
      val sqrt = Math.sqrt(1 + 8 * tn).round.toInt
      // square root term must be exact sqrt of tn and odd to make (-1 + sqrt) be divisible by 2
      if (sqrt * sqrt != (1 + 8 * tn) || sqrt % 2 != 1) None
      else Some((-1 + sqrt) / 2)
    }
  }

  /**
    * Checks is specified number is a triangle one
    */
  def isTriangleNumber(tn: Long): Boolean = getTriangleNumberIndex(tn).getOrElse(0) > 0
}
