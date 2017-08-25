package com.msokoryansky.MathUtils

/**
  * Legacy wrapper around PolygonalObject
  */
object HexagonalNumber {
  def hexagonalNumber(n: Int): Long = PolygonalNumber(6).number(n)

  def getHexagonalNumberIndex(hn: Long): Option[Long] = PolygonalNumber(6).indexOf(hn)

  def isHexagonalNumber(hn: Long): Boolean = PolygonalNumber(6).isNumber(hn)

  def hexagonalNumbers(n: Int = 1): Stream[Long] =PolygonalNumber(6).numbers(n)
}
