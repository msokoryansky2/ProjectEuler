package com.msokoryansky.MathUtils

object TriangleNumber {
  /**
    * Returns n triangle number given by formula tn = 1/2*n*(n+1)
    */
  def triangleNumber(n: Int): Long = {
    require(n > 0, "Must specify positive index for triangle number")
    n.toLong * (n + 1) / 2
  }

  /**
    * Returns whether specified number is a triangle one and its number in triangle series.
    * We derive this by solving for n given tn where
    * tn = 1/2*n*(n+1):
    * 1/2n*n + 1/2n = tn
    * n*n + n = 2tn
    * n*n + n - 2tn = 0
    * Using quadratic formula and only looking at positive solutions we get:
    * n = (-1 + SQRT(1 + 8tn) / 2)
    * If n is an integer, we know this number is a triangle one
    */
  def getTriangleNumberIndex(tn: Long): Option[Int] = {
    if (tn < 1) None
    else {
      val sqrt = Math.sqrt(1 + 8 * tn)
      if (!sqrt.isWhole() || (-1 + sqrt.toInt) % 2 != 0) None
      else Some((-1 + sqrt.toInt) / 2)
    }
  }

  /**
    * Checks is specified number is a triangle one
    */
  def isTriangleNumber(tn: Long): Boolean = getTriangleNumberIndex(tn).getOrElse(0) > 0

  def triangleNumbers(n: Int = 1): Stream[Long] = triangleNumber(n) #:: triangleNumbers(n + 1)
}
