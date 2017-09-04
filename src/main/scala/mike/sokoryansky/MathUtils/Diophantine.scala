package mike.sokoryansky.MathUtils

/**
  * Represents Diophantine equation of form a * pow(x, xPow) + b * pow(y, yPow) = c
  */
case class Diophantine(a: Long, xPow: Int, b: Long, yPow: Int, c: Long) {
  def y(x: Long): Option[Long] = {
    val cMinusX = c - a * Math.pow(x, xPow).toLong
    if (cMinusX % b != 0) None
    else if (Integer.isPow(cMinusX / b, yPow)) Some(Math.pow(cMinusX / b, 1 / yPow).toLong)
    else None
  }
  def yFind(ys: Stream[Long]): Option[Long] = ys.find(y => x(y).nonEmpty)
  def yFind: Option[Long] = yFind(Integer.ints(1))

  def x(y: Long): Option[Long] = {
    val cMinusY = c - b * Math.pow(y, yPow).toLong
    if (cMinusY % a != 0) None
    else if (Integer.isPow(cMinusY / a, xPow)) Some(Math.pow(cMinusY / a, 1 / xPow).toLong)
    else None
  }
  def xFind(xs: Stream[Long]): Option[Long] = xs.find(x => y(x).nonEmpty)
  def xFind: Option[Long] = xFind(Integer.ints(1))
}
