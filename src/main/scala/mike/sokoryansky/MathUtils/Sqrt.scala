package mike.sokoryansky.MathUtils

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

  /**
    * Square root of a number in string representation and number of decimal places to calculate to.
    * Returns pair of strings -- first one representing the integer portion, second the decimal portion.
    * Recycles HugePositiveInt's sqrt() function to get the result
    */
  def sqrt(value: String, decimalPlaces: Int): (String, String) = {
    require(BigInt(value) >= 0, "Must specify a non-negative number")
    require(decimalPlaces >= 0, "Must specify non-negative number of decimal places precision")
    // Tack on trailing zeroes to stay within integers. E.g.:
    // sqrt(2) to 5 decimal places works by taking sqrt(20,000,000,000) (with 5 * 2 zeroes)
    // and then taking 1st digit as the whole part and the rest as decimals
    val sqrt = HugePositiveInt( value + "0" * (decimalPlaces * 2)).sqrt
    val wholeDigits = (value.length + 1) / 2
    (sqrt.value.substring(0, wholeDigits), sqrt.value.substring(wholeDigits))
  }

  /**
    * Same as sqrt() above but using scala's BigInt instead of HugePositiveInt for speed
    */
  def sqrtFast(value: String, decimalPlaces: Int): (String, String) = {
    require(BigInt(value) >= 0, "Must specify a non-negative number")
    require(decimalPlaces >= 0, "Must specify non-negative number of decimal places precision")

    def sqrtBigInt(value: String): BigInt = {
      val valueBigInt = BigInt(value)
      // Number of digits in the answer is always ceiling of number of digits in the original / 2.
      // E.g. square roots of all 7- or 8-digit numbers are always 4 digits long.
      val digits = (value.length + 1) / 2

      def sqrtAcc(d: Long, lastGuess: Int, guessHistory: List[Int], acc: String): BigInt = {
        if (d > digits) if (acc.isEmpty) BigInt(0) else BigInt(acc)
        else {
          // Create full last guess number
          val lastGuessValue = BigInt(acc + lastGuess.toString + "0" * (digits - acc.length - 1))
          val lastGuessSqr = lastGuessValue * lastGuessValue
          lastGuessSqr.compare(valueBigInt) match {
            case 0 =>
              // Exact match, we found our sqrt
              lastGuessValue
            case over if over > 0 =>
              // Our sqrt is too big, will need to try with a lower guess -- but higher than next lowest existing guess
              val highestLowerGuess = guessHistory.find(m => m < lastGuess && !guessHistory.exists(n => n > m && n < lastGuess)).getOrElse(0)
              // if our highestLowerGuess is just 1 less than last guess, then that guess is the current digit and we move on
              if (lastGuess - highestLowerGuess <= 1) sqrtAcc(d + 1, 5, List(), acc + highestLowerGuess)
              else sqrtAcc(d, highestLowerGuess + (lastGuess - highestLowerGuess) / 2, lastGuess :: guessHistory, acc)
            case under if under < 0 =>
              // Our sqrt is too small, will need to try with a higher guess -- but lower than next highest existing guess
              val lowestHigherGuess = guessHistory.find(m => m > lastGuess && !guessHistory.exists(n => n < m && n > lastGuess)).getOrElse(10)
              // if our highestLowerGuess is just 1 less than last guess, then our last guess is the current digit and we move on
              if (lowestHigherGuess - lastGuess <= 1) sqrtAcc(d + 1, 5, List(), acc + lastGuess)
              else sqrtAcc(d, lastGuess + (lowestHigherGuess - lastGuess) / 2, lastGuess :: guessHistory, acc)
          }
        }
      }

      sqrtAcc(1, 5, List(), "")
    }

    // Tack on trailing zeroes to stay within integers. E.g.:
    // sqrt(2) to 5 decimal places works by taking sqrt(20,000,000,000) (with 5 * 2 zeroes)
    // and then taking 1st digit as the whole part and the rest as decimals
    val sqrt = sqrtBigInt(value + "0" * (decimalPlaces * 2))
    val wholeDigits = (value.length + 1) / 2
    (sqrt.toString.substring(0, wholeDigits), sqrt.toString.substring(wholeDigits))
  }
}
