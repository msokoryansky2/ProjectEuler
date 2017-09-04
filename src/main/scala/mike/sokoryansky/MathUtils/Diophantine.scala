package mike.sokoryansky.MathUtils

/**
  * Represents Diophantine equation of form a * pow(x, xPow) + b * pow(y, yPow) = c
  */
class Diophantine(a: Long, xPow: Int, b: Long, yPow: Int, c: Long) {
  def y(x: Long): Option[Long] = {
    val cMinusX = c - a * Math.pow(x, xPow).toLong
    if (cMinusX % b != 0) None
    else if (Integer.isPow(cMinusX / b, yPow)) Some(Math.pow(cMinusX / b, 1.toDouble / yPow).toLong)
    else None
  }
  def yFind(ys: Stream[Long]): Option[Long] = ys.find(y => x(y).nonEmpty)
  def yFind: Option[Long] = yFind(Integer.ints(1))

  def x(y: Long): Option[Long] = {
    val cMinusY = c - b * Math.pow(y, yPow).toLong
    if (cMinusY % a != 0) None
    else if (Integer.isPow(cMinusY / a, xPow)) Some(Math.pow(cMinusY / a, 1.toDouble / xPow).toLong)
    else None
  }
  def xFind(xs: Stream[Long]): Option[Long] = xs.find(x => y(x).nonEmpty)
  def xFind: Option[Long] = xFind(Integer.ints(1))
}

object Diophantine {
  def apply(a: Long, xPow: Int, b: Long, yPow: Int, c: Long) = new Diophantine(a, xPow, b, yPow, c)
}


/**
  * Logic for solving Pell equation: a special case of the quadratic Diophantine equation having the form
  * x^2-Dy^2=1,
  * where D>0 is a nonsquare natural number.
  *
  * See http://mathworld.wolfram.com/PellEquation.html
  */
class Pell private (a: Long, xPow: Int, b: Long, yPow: Int, c: Long) extends Diophantine(a, xPow, b, yPow, c) {
  require(b < 0, "Must specify positive parameter D for Pell Equation x^2-Dy^2=1")
  lazy val sqrtD: CF = CF.sqrt(0 - b)
  lazy val r: Int = sqrtD.fractionRepeat.size - 1
  lazy val convergentIndex: Int = if (r % 2 == 1) r else 2 * r + 1
  lazy val convergentFraction: Fraction = sqrtD.toFraction(convergentIndex)
  lazy val xFirst: BigInt = convergentFraction.num
  lazy val yFirst: BigInt = convergentFraction.denom
}

object Pell {
  def apply(d: Long) = new Pell(1, 2, 0 - d, 2, 1)
}
