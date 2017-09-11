package mike.sokoryansky.MathUtils

/**
  * See https://en.wikipedia.org/wiki/Farey_sequence
  */
object FareySequence {
  /**
    * Number of proper reduced fractions with denominator of n
    */
  def count(n: Long): Long = {
    require(n > 1, "Must specify integer > 1")
    Prime.totient(n)
  }

  /**
    * Number of proper reduced fractions with denominators of 2 through n
    */
  def count2ToN(n: Long): Long = {
    require(n > 1, "Must specify integer > 1")
    Prime.totient1toN(n).values.sum - 1
  }
}
