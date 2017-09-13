package mike.sokoryansky.MathUtils

/**
  * See https://en.wikipedia.org/wiki/Farey_sequence
  */
object FareySeq {
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

  /**
    * Numerators of all Farey Sequence fractions for specified denominator
    */
  def numerators(n: Long): Seq[Long] =
    Prime.relativePrimes(n)

  /**
    * Numerators of all Farey Sequence fractions for all denominators from 2 to specified number
    */
  def numerators2ToN(n: Long): Map[Long, Seq[Long]] =
    Prime.relativePrimesThroughN(n)

  /**
    * Numerators of all Farey Seq fractions that meet specified predicate when formed as a Fraction
    */
  def numeratorsFilter(n: Long, p: Fraction => Boolean): Seq[Long] =
    numerators(n).filter(num => p(Fraction(num, n)))

  /**
    * Numerators of all Farey Seq fractions from 2 to specified number
    * that meet specified predicate when formed as a Fraction
    */
  def numeratorsFilter2ToN(n: Long, p: Fraction => Boolean): Map[Long, Seq[Long]] =
    numerators2ToN(n).map(fs => fs._1 -> fs._2.filter(num => p(Fraction(num, fs._1))))

  /**
    * Numerators of all Fareq Seq fractions for specified denom that are less than comparison fraction (optimized)
    */
  def numeratorsLessThanFraction(n: Long, f: Fraction): Seq[Long] = {
    val fNum = f.num.toLong
    val fDenom = f.denom.toLong
    val upper = if ((n * fNum) % fDenom == 0) (n * fNum) / fDenom - 1 else (n * fNum) / fDenom
    (1L to upper).filter(i => Integer.gcd(i, n) == 1)
  }

  /**
    * Numerators of all Fareq Seq fractions for denoms from 2 to specified number
    * that are less than comparison fraction (optimized)
    */
  def numeratorsLessThanFraction2ToN(n: Long, f: Fraction): Map[Long, Seq[Long]] = {
    val fNum = f.num.toLong
    val fDenom = f.denom.toLong
    (2L to n).map(n2 => n2 -> {
      val upper = if ((n2 * fNum) % fDenom == 0) (n2 * fNum) / fDenom - 1 else (n2 * fNum) / fDenom
      (1L to upper).filter(i => Integer.gcd(i, n2) == 1)
    }).toMap.filterNot(_._2.isEmpty)
  }
}
