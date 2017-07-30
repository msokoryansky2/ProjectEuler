package com.msokoryansky.MathUtils

object Triangle {
  def triangleNumbers(lastTriangleIndex: Long, lastTriangleNumber: Long): Stream[Long] = {
    val nextTriangleNumber = lastTriangleNumber + lastTriangleIndex + 1
    nextTriangleNumber #:: triangleNumbers(lastTriangleIndex + 1, nextTriangleNumber)
  }
}
