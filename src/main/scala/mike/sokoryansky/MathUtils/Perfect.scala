package mike.sokoryansky.MathUtils

object Perfect {
  def perfection(i: Long): Perfection.Value = {
    require(i > 0, "Perfection only applies to positive integers")
    i - Integer.divisors(i).filter(_ != i).sum match {
      case n if n < 0 => Perfection.Abundant
      case n if n == 0 => Perfection.Perfect
      case n if n > 0 => Perfection.Deficient
    }
  }
}

object Perfection extends Enumeration {
  val Perfect = Value(0)
  val Abundant = Value(1)
  val Deficient = Value(-1)
}