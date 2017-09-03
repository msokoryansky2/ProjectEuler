package mike.sokoryansky.MathUtils

/**
  * Represents Diophantine equation of form a * pow(x, xPow) + b * pow(y, yPow) = c
  */
case class Diophantine(a: Long, xPow: Int, b: Long, yPow: Int, c: Long) {
  def solveY(x: Long): Option[Long] = {
    val cMinusX = c - a * Math.pow(x, xPow).toLong
    if (cMinusX % b != 0) None
    else if (Integer.isPow(cMinusX / b, yPow)) Some(Math.pow(cMinusX / b, 1 / yPow).toLong)
    else None
  }

  def solveX(y: Long): Option[Long] = {
    val cMinusY = c - b * Math.pow(y, yPow).toLong
    if (cMinusY % a != 0) None
    else if (Integer.isPow(cMinusY / a, xPow)) Some(Math.pow(cMinusY / a, 1 / xPow).toLong)
    else None
  }

  def maxX: Long = {

  }
}
