package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Sqrt {
  /**
    * Returns sqrt(2) expansion in form of (numerator, denominator) after specified number of expansion iterations
    * Eg:
    * sqrtOf2Expansion(1) = 1 + 1/2 = (3, 2)
    * sqrtOf2Expansion(2) = 1 + 1/(2 + 1/2) = (7, 5)
    * sqrtOf2Expansion(3) = 1 + 1/(2 + 1/(2 + 1/2)) = (17, 12)
    *
    * Optionally, we can provide (to be taken at face value) fractional result of previous expansion iteration,
    * so that current iteration can be computed in one go, isntead of recursively starting with iteration #1.
    */
  def sqrtOf2Fraction(iters: Int, lastFraction: Option[(HugePositiveInt, HugePositiveInt)] = None,
                      fractionOnly: Boolean = false): (HugePositiveInt, HugePositiveInt) = {
    require(iters > 0, "Must specify positive number of square root of 2 expansion iterations")
    def sqrtOf2FractionAcc(iter: Int, lastFraction: Option[(HugePositiveInt, HugePositiveInt)]):
                            (HugePositiveInt, HugePositiveInt) = {
      if (iter > iters) lastFraction.get
      else {
        val expansion = sqrtOf2FractionAcc(iter + 1, lastFraction)
        (expansion._2, expansion._2 * HugePositiveInt(2) + expansion._1)
      }
    }
    val fraction = lastFraction match {
      case Some(x) => sqrtOf2FractionAcc(iters, lastFraction)
      case None => sqrtOf2FractionAcc(2, Some((HugePositiveInt(1), HugePositiveInt(2))))
    }
    (if (fractionOnly) fraction._1 else fraction._2 + fraction._1, fraction._2)
  }

  /**
    * All expansions of sqrt(2) from 1 to iters, in form of a map iter -> sqrtOf2Fraction(iter)
    */
  def sqrtOf2Fractions(iters: Int, fractionOnly: Boolean = false): Map[Int, (HugePositiveInt, HugePositiveInt)]= {
    require(iters > 0, "Must specify positive number of square root of 2 expansion iterations")
    @tailrec def sqrtOf2FractionsAcc(iter: Int, acc: Map[Int, (HugePositiveInt, HugePositiveInt)])
            : Map[Int, (HugePositiveInt, HugePositiveInt)] = {
      if (iter > iters) acc
      else sqrtOf2FractionsAcc(iter + 1, acc + (iter -> sqrtOf2Fraction(iter, Some(acc(iter - 1)), fractionOnly = true)))
    }
    val fractions = sqrtOf2FractionsAcc(2, Map(1 -> sqrtOf2Fraction(1, None, fractionOnly = true)))
    if (fractionOnly) fractions else fractions.map(f => f._1 -> (f._2._1 + f._2._2, f._2._2))
  }
}
