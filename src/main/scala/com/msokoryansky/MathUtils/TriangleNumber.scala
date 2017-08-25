package com.msokoryansky.MathUtils

/**
  * Legacy wrapper around PolygonalObject
  */
object TriangleNumber {
  def triangleNumber(n: Int): Long = PolygonalNumber(3).number(n)

  def getTriangleNumberIndex(tn: Long): Option[Long] = PolygonalNumber(3).indexOf(tn)

  def isTriangleNumber(tn: Long): Boolean = PolygonalNumber(3).isNumber(tn)

  def triangleNumbers(n: Int = 1): Stream[Long] = PolygonalNumber(3).numbers(n)
}
